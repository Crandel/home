#!/bin/bash

while [ 1 ]
do
  # Check whether or not Xorg exists
  if pgrep Xorg; then
    sleep 1
    break;
  fi
  echo -n "."
  sleep 0.1
done
