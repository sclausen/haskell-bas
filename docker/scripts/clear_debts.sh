#!/bin/bash
DB_PATH="/etc/bas/bas.db"
SQLITE_ARGS="--header --column"

sudo sqlite3 $SQLITE_ARGS $DB_PATH "SELECT id, username FROM user ORDER BY id;"

printf "\nplease select user id:"
read userId

if [ -z "${userId}" ]; then
  echo "UserId is unset or set to the empty string"
else
 sudo sqlite3 $DB_PATH "UPDATE user SET debts = 0 WHERE id = $userId"
 echo "debts cleared"
 sudo sqlite3 $SQLITE_ARGS $DB_PATH "SELECT * FROM user WHERE id = $userId"
fi
