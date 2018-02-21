PRAGMA foreign_keys=off;

BEGIN TRANSACTION;

UPDATE stock SET price = cast(price*100 AS INTEGER);

ALTER TABLE stock RENAME TO stock_tmp;

CREATE TABLE IF NOT EXISTS stock (id INTEGER PRIMARY KEY, label TEXT NOT NULL UNIQUE, price INTEGER NOT NULL, amount INTEGER NOT NULL)

INSERT INTO stock (id, label, price, amount)
  SELECT id, label, price, amount
  FROM stock_tmp;

COMMIT;

PRAGMA foreign_keys=on;