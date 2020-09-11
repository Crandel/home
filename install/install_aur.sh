#!/usr/bin/bash

source export_vars.sh

if ! command_exists yay ; then
  $pacman -S git
  git clone https://aur.archlinux.org/yay.git /tmp/yay
  cd /tmp/yay
  makepkg -si
fi

$yay -S adb-sync-git \
        grive-git \
        inxi \
        j4-dmenu-desktop \
        libinput-gestures \
        simple-mtpfs \
        shadowfox-updater
