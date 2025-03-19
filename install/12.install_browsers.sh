#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "firefox"
  "vivaldi"
  "qutebrowser"
  "telegram-desktop"
  "thunderbird"
  "rssguard"
  "systray-x-common"
)

install_pacman $packages

declare -a y_pkgs=(
  "vieb-bin"
  "librewolf-bin"
)

install_yay $y_pkgs
