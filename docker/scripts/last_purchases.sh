#!/bin/bash
DB_PATH="/etc/bas/bas.db"
SQLITE_ARGS="--header --column"

sqlite3 $SQLITE_ARGS $DB_PATH "SELECT user.username, stock.label, purchase.boughtAt FROM purchase INNER JOIN user ON user.id = userId INNER JOIN stock ON stock.id = stockId ORDER BY purchase.boughtAt DESC;"