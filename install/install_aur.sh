#!/usr/bin/bash

pacman -S git
git clone https://aur.archlinux.org/yay.git /tmp/yay
cd /tmp/yay
makepkg -si

yay -S simple-mtpfs \
       shadowfox-updater
