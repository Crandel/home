#!/usr/bin/bash

source export_vars.sh
export WINEARCH=win32
declare -a packages=(
  "wine"
  "winetricks"
  "wine-mono"
  "wine-gecko"
  "wine-nine"
  "vkd3d"
  "qt6-tools"
  "zenity"
)
install_pacman $packages

declare -a y_pkgs=(
  "vcmi"
)

install_yay $y_pkgs
