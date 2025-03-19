#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "adobe-source-code-pro-fonts"
  "capitaine-cursors"
  "deepen-sound-theme"
  "fontconfig"
  "gtk-engine-murrine"
  "kvantum"
  "noto-fonts-emoji"
  "otf-droid-nerd"
  "papirus-icon-theme"
  "ttf-dejavu-nerd"
  "ttf-firacode-nerd"
  "ttf-hack-nerd"
  "ttf-iosevka-nerd"
  "ttf-jetbrains-mono-nerd"
  "ttf-liberation"
  "ttf-nerd-fonts-symbols-mono"
  "ttf-noto-nerd"
  "ttf-roboto"
  "ttf-terminus-nerd"
)

install_pacman $packages
