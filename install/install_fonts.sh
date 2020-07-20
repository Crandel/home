#!/usr/bin/bash

source export_vars.sh

$pacman -S awesome-terminal-fonts \
           capitaine-cursors \
           noto-fonts \
           noto-fonts-extra \
           perl-font-ttf \
           terminus-font \
           ttf-anonymous-pro \
           ttf-dejavu \
           ttf-fira-mono \
           ttf-fira-sans \
           ttf-hack \
           ttf-liberation \
           ttf-nerd-fonts-symbols \
           ttf-ubuntu-font-family

$yay -S gruvbox-dark-gtk \
        gruvbox-dark-icons-gtk \
        gruvbox-icon-theme
