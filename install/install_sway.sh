#!/usr/bin/bash

source export_vars.sh

$pacman -S bemenu \
           bemenu-wlroots \
           kwayland \
           kwayland-integration \
           light \
           mako \
           plasma-wayland-protocols \
           python-i3ipc \
           qt5-wayland \
           slurp \
           sway \
           swaybg \
           swayidle \
           swaylock \
           wayland-protocols \
           wl-clipboard \
           wlroots \
           xorg-server-xwayland

$yay -S clipman \
        grimshot \
        j4-dmenu-desktop \
        swaykbdd \
        wev
