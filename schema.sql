--*- mode: sql; product: postgres -*--

BEGIN TRANSACTION;

-- For PostgreSQL 9.1 upward, we should consider using enum types
-- instead of VARCHARs with CHECK constraints:
--
-- CREATE TYPE user_status AS
--   ENUM ('pending', 'approved', 'trusted', 'deferred', 'disabled', 'admin');
--
-- CREATE TYPE comment_status AS
--   ENUM ('pending', 'approved', 'trusted', 'deferred', 'spam', 'rejected');
--
-- CREATE TYPE text_format AS
--   ENUM ('html', 'text');


CREATE TABLE users(
  id      SERIAL  NOT NULL,
  name    VARCHAR,
  status  VARCHAR NOT NULL,
  email   VARCHAR,
  website VARCHAR,
  PRIMARY KEY (id),
  CHECK (status IN ('pending', 'approved', 'trusted', 'deferred', 'disabled',
                    'admin', 'visitor'))
);

CREATE TABLE passwords(
  "user"   INTEGER NOT NULL,
  password VARCHAR NOT NULL,
  PRIMARY KEY ("user", password),
  FOREIGN KEY ("user") REFERENCES users
);  

CREATE TABLE openids(
  "user" INTEGER NOT NULL,
  openid VARCHAR NOT NULL,
  PRIMARY KEY ("user", openid),
  FOREIGN KEY ("user") REFERENCES users
);  

CREATE TABLE login_certificates(
  "user"      INTEGER NOT NULL,
  certificate BYTEA  NOT NULL,
  PRIMARY KEY ("user", certificate),
  FOREIGN KEY ("user") REFERENCES users
);

CREATE TABLE article_types(
  id            SERIAL NOT NULL,
  name          VARCHAR,
  page_template VARCHAR,
  PRIMARY KEY (id)
);

CREATE TABLE articles(
  id   SERIAL  NOT NULL,
  type INTEGER NOT NULL,
  PRIMARY KEY (id),
  FOREIGN KEY (type) REFERENCES article_types
);

CREATE TABLE article_aliases(
  alias   VARCHAR NOT NULL,
  article INTEGER NOT NULL,
  PRIMARY KEY (alias),
  FOREIGN KEY (article) REFERENCES articles
);

CREATE TABLE comments(
  id        SERIAL  NOT NULL,
  article   INTEGER NOT NULL,
  global_id VARCHAR,
  PRIMARY KEY (id),
  FOREIGN KEY (article) REFERENCES articles
);

CREATE TABLE article_revisions(
  id        SERIAL    NOT NULL,
  article   INTEGER   NOT NULL,
  date      TIMESTAMP DEFAULT now(),
  title     VARCHAR   NOT NULL,
  content   VARCHAR   NOT NULL,
  author    INTEGER,
  format    VARCHAR   NOT NULL,
  status    VARCHAR   NOT NULL,
  global_id VARCHAR,
  PRIMARY KEY (id),
  FOREIGN KEY (article) REFERENCES articles,
  FOREIGN KEY (author)  REFERENCES users,
  CHECK (format IN ('html')),
  CHECK (status IN ('draft', 'published', 'syndicated')),
  UNIQUE (global_id)
);

CREATE TABLE article_revision_characteristics(
  revision       INTEGER NOT NULL,
  characteristic VARCHAR NOT NULL,
  value          VARCHAR,
  FOREIGN KEY (revision) REFERENCES article_revisions,
  UNIQUE (revision, characteristic, value)
);

CREATE TABLE article_revision_parenthood(
  parent INTEGER NOT NULL,
  child  INTEGER NOT NULL,
  PRIMARY KEY (parent, child),
  FOREIGN KEY (parent) REFERENCES article_revisions,
  FOREIGN KEY (child)  REFERENCES article_revisions
);

CREATE TABLE comment_revisions(
  id                   SERIAL    NOT NULL,
  comment              INTEGER   NOT NULL,
  date                 TIMESTAMP DEFAULT now(),
  content              VARCHAR   NOT NULL,
  author               INTEGER,
  submitter_ip         INET,
  submitter_user_agent VARCHAR,
  format               VARCHAR   NOT NULL,
  status               VARCHAR   NOT NULL,
  article_revision     INTEGER,
  PRIMARY KEY (id),
  FOREIGN KEY (comment) REFERENCES comments,
  FOREIGN KEY (author)  REFERENCES users,
  CHECK (status IN ('pending', 'approved', 'trusted', 'deferred', 'spam',
                    'rejected')),
  CHECK (format IN ('text'))
);

CREATE TABLE categories(
  id   SERIAL  NOT NULL,
  name VARCHAR NOT NULL,
  PRIMARY KEY (id)
);

CREATE TABLE category_inclusions(
  category      INTEGER NOT NULL,
  supercategory INTEGER NOT NULL,
  PRIMARY KEY (category, supercategory),
  FOREIGN KEY (category)      REFERENCES categories,
  FOREIGN KEY (supercategory) REFERENCES categories
);

CREATE TABLE article_category_memberships(
  article  INTEGER NOT NULL,
  category VARCHAR NOT NULL,
  PRIMARY KEY (article, category),
  FOREIGN KEY (article) REFERENCES articles
);

CREATE TABLE user_permissions(
  "user"     INTEGER NOT NULL,
  permission VARCHAR NOT NULL,
  status     BOOLEAN,
  PRIMARY KEY ("user", permission),
  FOREIGN KEY ("user") REFERENCES users
);

CREATE TABLE user_settings(
  "user"  INTEGER NOT NULL,
  setting VARCHAR NOT NULL,
  value   VARCHAR,
  PRIMARY KEY ("user", setting),
  FOREIGN KEY ("user") REFERENCES users
);


----

CREATE TYPE characteristic AS (
  characteristic VARCHAR,
  value          VARCHAR
);

CREATE TYPE characteristic_list AS (
  characteristics characteristic[]
);

CREATE OR REPLACE FUNCTION article_revisions_for_characteristics_with_fixed_ones(
  article INTEGER,
  characteristics characteristic_list[],
  fixed_characteristics characteristic_list
) RETURNS INTEGER[] AS $BODY$
DECLARE
  query              VARCHAR        := $$SELECT id FROM article_revisions
                                          WHERE article = $$
                                       || quote_literal(article) ||
                                       $$   AND status IN ('published', 'syndicated')$$;
  fchar              RECORD;
  revisions          INTEGER[];
  specific_revisions INTEGER[];
  dummy              INTEGER;
BEGIN
  FOR fchar IN SELECT unnest(fixed_characteristics.characteristics) AS val LOOP
    query := query || $$AND EXISTS
                          (SELECT 1
                             FROM article_revision_characteristics
                            WHERE revision = article_revisions.id
                              AND characteristic = $$ || quote_literal((fchar.val::characteristic).characteristic) || $$
                              AND value = $$ || quote_literal((fchar.val::characteristic).value) || $$)$$;
  END LOOP;
  query := query || $$ORDER BY date DESC$$;
  EXECUTE ('SELECT array(' || query || ')') INTO revisions;
  IF array_length(revisions, 1) > 0 THEN
    IF array_length(characteristics, 1) > 0 THEN
      FOR fchar IN SELECT unnest(characteristics[1].characteristics) AS val LOOP
        specific_revisions := article_revisions_for_characteristics_with_fixed_ones(
          article,
          characteristics[2:array_upper(characteristics, 1)],
          ROW(array_prepend(fchar.val, fixed_characteristics.characteristics))::characteristic_list
        );
        IF array_length(specific_revisions, 1) > 0 THEN
          RETURN specific_revisions;
        END IF;
      END LOOP;
      RETURN revisions;
    ELSE
      RETURN revisions;
    END IF;
  ELSE
    RETURN ARRAY[]::INTEGER[];
  END IF;
END
$BODY$ LANGUAGE plpgsql;

CREATE OR REPLACE FUNCTION article_revisions_for_characteristics(
  article INTEGER,
  characteristics characteristic_list[]
) RETURNS SETOF INTEGER AS $$
BEGIN
  RETURN QUERY SELECT unnest(article_revisions_for_characteristics_with_fixed_ones(article, characteristics, ROW(ARRAY[]::characteristic[])::characteristic_list));
END
$$ LANGUAGE plpgsql;

-- Usage example:
--   SELECT article_revisions_for_characteristics(70, ARRAY[ROW(ARRAY[ROW('language', 'de')::characteristic])]::characteristic_list[]);


CREATE OR REPLACE FUNCTION older_revision(
  IN article_revisions,
  IN article_revisions,
  OUT article_revisions
) AS $$
  SELECT $1 WHERE $1.date < $2.date OR $2.date IS NULL
  UNION
  SELECT $2 WHERE $1.date >= $2.date OR $1.date IS NULL
  UNION
  SELECT $1 WHERE $1.date IS NULL AND $2.date IS NULL
$$ LANGUAGE SQL;

CREATE AGGREGATE oldest_revision (article_revisions) (
  SFUNC = older_revision,
  STYPE = article_revisions
);


CREATE OR REPLACE FUNCTION more_recent_revision(
  IN article_revisions,
  IN article_revisions,
  OUT article_revisions
) AS $$
  SELECT $1 WHERE $1.date > $2.date OR $2.date IS NULL
  UNION
  SELECT $2 WHERE $1.date <= $2.date OR $1.date IS NULL
  UNION
  SELECT $1 WHERE $1.date IS NULL AND $2.date IS NULL
$$ LANGUAGE SQL;


CREATE AGGREGATE most_recent_revision (article_revisions) (
  SFUNC = more_recent_revision,
  STYPE = article_revisions
);


CREATE VIEW article_comment_counts AS
  SELECT a.id       AS article,
         count(c.*) AS comment_count
    FROM articles a
    LEFT JOIN comments c
      ON c.article = a.id
     AND EXISTS (SELECT *
                   FROM comment_revisions
                  WHERE comment = c.id
                    AND status IN ('approved', 'trusted'))
   GROUP BY a.id;


CREATE VIEW article_publishing_dates AS
  SELECT article   AS article,
         min(date) AS publishing_date
    FROM article_revisions
   WHERE status IN ('published', 'syndicated')
   GROUP BY article;


COMMIT;
