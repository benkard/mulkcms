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
  id     SERIAL  NOT NULL,
  name   VARCHAR,
  status VARCHAR NOT NULL,
  PRIMARY KEY (id),
  CHECK (status IN ('pending', 'approved', 'trusted', 'deferred', 'disabled',
                    'admin'))
);

CREATE TABLE passwords(
  "user"   INTEGER NOT NULL,
  password VARCHAR NOT NULL,
  FOREIGN KEY ("user") REFERENCES users
);  

CREATE TABLE openids(
  "user" INTEGER NOT NULL,
  openid VARCHAR NOT NULL,
  FOREIGN KEY ("user") REFERENCES users
);  

CREATE TABLE login_certificates(
  "user"      INTEGER NOT NULL,
  certificate BYTEA NOT NULL,
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
  id      SERIAL  NOT NULL,
  article INTEGER NOT NULL,
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
  FOREIGN KEY (revision) REFERENCES article_revisions
);

CREATE TABLE article_revision_parenthood(
  parent_id INTEGER NOT NULL,
  child_id  INTEGER NOT NULL,
  FOREIGN KEY (parent_id) REFERENCES article_revisions,
  FOREIGN KEY (child_id)  REFERENCES article_revisions
);

CREATE TABLE comment_revisions(
  id               SERIAL    NOT NULL,
  comment          INTEGER   NOT NULL,
  date             TIMESTAMP DEFAULT now(),
  content          VARCHAR   NOT NULL,
  author           INTEGER,
  format           VARCHAR   NOT NULL,
  status           VARCHAR   NOT NULL,
  article_revision INTEGER,
  PRIMARY KEY (id),
  FOREIGN KEY (comment) REFERENCES comments,
  FOREIGN KEY (author)  REFERENCES users,
  CHECK (status IN ('pending', 'approved', 'trusted', 'deferred', 'spam',
                    'rejected')),
  CHECK (format IN ('text'))
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

COMMIT;
