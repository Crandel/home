#!/usr/bin/env bash

swaynag -t warning \
        -m 'Hello. Do you really want to exit? This will end your Wayland session.' \
        -b 'Suspend' 'systemctl suspend' \
        -b 'Lock' '$lock' \
        -b 'Shutdown' 'systemctl -i poweroff' \
        -b 'Reboot' 'systemctl -i reboot' \
        -b 'Yes, exit' 'swaymsg exit' \
        --button-background=#FF9100 \
        --button-border-size=3px \
        --border=#FCC99D \
        --text=#092E47 \
        --font=FiraCode Nerd Font Mono 16 \
        --background=#D65D0E
