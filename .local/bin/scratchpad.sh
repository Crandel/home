#!/bin/sh

exctble=$1
app_id=$2


msg=$(swaymsg -p [app_id="$app_id"] scratchpad show)
if [ ! -z "$msg" ]; then
  echo "msg: $msg"
  swaymsg exec "$exctble", move to scratchpad
  swaymsg [app_id="$app_id"] scratchpad show
fi
swaymsg [app_id="$app_id"] border none, move position center, resize set width 100 ppt height 100 ppt
