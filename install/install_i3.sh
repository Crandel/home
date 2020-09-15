#!/usr/bin/bash

source export_vars.sh

$pacman -S bemenu \
           bemenu-x11 \
           clipmenu \
           dunst \
           feh \
           i3-wm \
           i3lock \
           network-manager-applet \
           numlockx \
           picom

$yay -S j4-dmenu-desktop \
        kbdd-git
