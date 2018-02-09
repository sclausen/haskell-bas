#!/bin/bash
for username in "$@"
do
  useradd -m -s /bin/bash $username
  usermod -p '*' $username
  usermod -aG bas $username
  mkdir -p /home/$username/.ssh
  touch /home/$username/.ssh/authorized_keys
  chown -R $username:$username /home/$username/.ssh
  chmod 700 /home/$username/.ssh
  chmod 600 /home/$username/.ssh/authorized_keys
done
