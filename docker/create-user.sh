#!/bin/bash
useradd -m -s /bin/bash $1
usermod -p '*' $1
usermod -aG bas $1
touch /home/$1/.ssh/authorized_keys
chown -R $1:$1 /home/$1/.ssh
chmod 700 /home/$1/.ssh
chmod 600 /home/$1/.ssh/authorized_keys