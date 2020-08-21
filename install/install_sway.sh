#!/usr/bin/bash

source export_vars.sh

$pacman -S bemenu \
           bemenu-wlroots \
           kwayland \
           kwayland-integration \
           mako \
           plasma-wayland-protocols \
           qt5-wayland \
           slurp \
           sway \
           swaybg \
           swaylock \
           light \
           wayland-protocols \
           wl-clipboard \
           wlroots \
           xorg-server-xwayland \
           python-i3ipc

$yay -S clipman \
        grimshot \
        j4-dmenu-desktop
