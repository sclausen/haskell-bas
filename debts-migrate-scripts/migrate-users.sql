PRAGMA foreign_keys=off;

BEGIN TRANSACTION;

UPDATE user SET debts = cast(debts*100 AS INTEGER);

ALTER TABLE user RENAME TO user_tmp;

CREATE TABLE IF NOT EXISTS user (id INTEGER PRIMARY KEY, username TEXT NOT NULL UNIQUE, debts INTEGER NOT NULL);

INSERT INTO user (id, username, debts)
  SELECT id, username, debts
  FROM user_tmp;

COMMIT;

PRAGMA foreign_keys=on;