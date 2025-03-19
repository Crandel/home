#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
   "bzip2"
   "gzip"
   "i7z"
   "p7zip"
   "tar"
   "zip"
   "unzip"
   "unrar"
)

install_pacman $packages

declare -a y_pkgs=(
  "archivemount"
  "rar2fs"
)

install_yay $y_pkgs
