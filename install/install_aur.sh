#!/usr/bin/bash

pacman -S git
git clone https://aur.archlinux.org/yay.git /tmp/yay
cd /tmp/yay
makepkg -si

yay -S adb-sync-git \
       grive-git \
       inxi \
       intellij-idea-community-edition-no-jre \
       j4-dmenu-desktop \
       k9s \
       libinput-gestures \
       simple-mtpfs \
       shadowfox-updater
