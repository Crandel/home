#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "cliphist"
  "fuzzel"
  "i3status-rust"
  "imv"
  "j4-dmenu-desktop"
  "kwayland"
  "kwayland-integration"
  "light"
  "mako"
  "plasma-wayland-protocols"
  "python-i3ipc"
  "qt6-wayland"
  "slurp"
  "sway"
  "swaybg"
  "swayidle"
  "swaylock"
  "wayland-protocols"
  "wev"
  "wl-clipboard"
  "wlroots"
  "wtype"
  "xorg-server-xwayland"
)

install_pacman $packages

declare -a y_pkgs=(
  "i3keys"
  "swaykbdd"
  "bemoji"
)

install_yay $y_pkgs
