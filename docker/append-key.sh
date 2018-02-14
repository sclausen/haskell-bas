#!/bin/bash
cat $1.pub | (ssh root@172.17.0.2 "cat >> /root/.ssh/keys/$1.pub")
