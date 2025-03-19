#!/usr/bin/env bash

kill=$1
timeout=$2
rsm='dpms_switcher.sh'
if [ ! -z $kill ]; then
  rsm="$rsm kill"
fi

if [ -z $timeout ]; then
  timeout=300
fi

echo "idle.sh; timeout=$timeout; rsm=$rsm" >> /tmp/idle.log

swayidle \
  timeout $timeout 'swaymsg "output * dpms off"' \
  resume "$rsm" &

echo "$!" > /tmp/swayidle.pid
