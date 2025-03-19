#!/usr/bin/env bash

# run this in sway as:
# exec wl-paste -t text --watch cliphist.sh

app_id=$( swaymsg -t get_tree | jq -r '.. | select(.type?) | select(.focused==true)|.app_id'  )
if [[ $app_id != "org.keepassxc.KeePassXC" ]]; then
    cliphist store --max-items=1000 1>> "$HOME/.local/log/cliphist.log" 2>&1
fi
