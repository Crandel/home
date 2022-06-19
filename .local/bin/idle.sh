#!/usr/bin/env bash

kill=$1
timeout=$2
rsm='dpms_switcher.sh'
if [ ! -z $kill ]; then
  rsm="$rsm kill"
fi

if [ -z $timeout ]; then
  timeout=180
fi

echo "idle.sh; timeout=$timeout; rms=$rms" >> /tmp/idle.log

swayidle \
  timeout $timeout 'swaymsg "output * dpms off"' \
  resume "$rsm" &

echo "$!" > /tmp/swayidle.pid

exec swayidle timeout 30 "makoctl set-mode away" resume "makoctl set-mode default" &
