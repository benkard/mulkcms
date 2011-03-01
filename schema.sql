BEGIN TRANSACTION;

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
  id       INTEGER NOT NULL,
  password VARCHAR NOT NULL,
  PRIMARY KEY ("user", id),
  FOREIGN KEY ("user") REFERENCES users
);  

CREATE TABLE openids(
  "user" INTEGER NOT NULL,
  id     INTEGER NOT NULL,
  openid VARCHAR NOT NULL,
  PRIMARY KEY ("user", id),
  FOREIGN KEY ("user") REFERENCES users
);  

CREATE TABLE login_certificates(
  "user"      INTEGER NOT NULL,
  id          INTEGER NOT NULL,
  certificate BYTEA NOT NULL,
  PRIMARY KEY ("user", id),
  FOREIGN KEY ("user") REFERENCES users
);

CREATE TABLE articles(
  id SERIAL NOT NULL,
  PRIMARY KEY (id)
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
  PRIMARY KEY (id, article),
  FOREIGN KEY (article) REFERENCES articles
);

CREATE TABLE article_revisions(
  article   INTEGER   NOT NULL,
  id        INTEGER   NOT NULL,
  date      TIMESTAMP DEFAULT now(),
  title     VARCHAR   NOT NULL,
  content   VARCHAR   NOT NULL,
  author    INTEGER,
  format    varchar   NOT NULL,
  PRIMARY KEY (article, id),
  FOREIGN KEY (article) REFERENCES articles,
  FOREIGN KEY (author)  REFERENCES users,
  CHECK (format IN ('html'))
);

CREATE TABLE article_revision_characteristics(
  article        INTEGER NOT NULL,
  revision       INTEGER NOT NULL,
  characteristic VARCHAR NOT NULL,
  value          VARCHAR,
  FOREIGN KEY (article, revision) REFERENCES article_revisions
);

CREATE TABLE article_revision_parenthood(
  article   INTEGER NOT NULL,
  parent_id INTEGER NOT NULL,
  child_id  INTEGER NOT NULL,
  FOREIGN KEY (article, parent_id) REFERENCES article_revisions,
  FOREIGN KEY (article, child_id)  REFERENCES article_revisions
);

CREATE TABLE comment_revisions(
  article          INTEGER   NOT NULL,
  comment_id       INTEGER   NOT NULL,
  id               INTEGER   NOT NULL,
  date             TIMESTAMP DEFAULT now(),
  content          VARCHAR   NOT NULL,
  author           INTEGER,
  format           VARCHAR   NOT NULL,
  status           VARCHAR   NOT NULL,
  article_revision INTEGER,
  PRIMARY KEY (article, comment_id, id),
  FOREIGN KEY (article, comment_id) REFERENCES comments,
  FOREIGN KEY (author)              REFERENCES users,
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
