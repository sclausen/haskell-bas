#!/bin/bash
for username in "$@"
do
  useradd -m -s /bin/bash $username
  usermod -p '*' $username
  usermod -aG bas $username
done
