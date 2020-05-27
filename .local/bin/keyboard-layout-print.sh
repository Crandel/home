#!/bin/bash

swaymsg -t get_inputs | jq -r \
    "first(.[]|select(.type == \"keyboard\")) \
    | .xkb_active_layout_name \
    | .[0:2] \
    | ascii_upcase"

