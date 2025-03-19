#!/usr/bin/env bash

count=0
while ! ping -c 1 8.8.8.8 > /dev/null ; do
  sleep 1
  count=$((count + 1))
  if [ "$count" -eq 1000 ];then
     exit 1
  fi
done
