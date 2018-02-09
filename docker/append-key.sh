#!/bin/bash
cat $1.pub | (ssh root@172.17.0.2 "cat >> /home/$1/.ssh/authorized_keys")
