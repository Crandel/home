#!/usr/bin/env bash

kill=$1
echo "dpms; kill $kill exists" >> /tmp/idle.log

pid_file=/tmp/swayidle.pid

if [ -n "$kill" ] && [ -f $pid_file ]; then
  pid=$(cat $pid_file)
  echo "dpms; pid from file $pid" >> /tmp/idle.log
  swaymsg "output * dpms on"; kill -9 "$pid"
else
  swaymsg "output * dpms on"
fi

rm /tmp/swayidle.pid
