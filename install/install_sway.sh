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
           wayland-protocols \
           wl-clipboard \
           wlroots \
           xorg-server-xwayland

$yay -S clipman \
        grimshot
