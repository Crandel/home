#!/usr/bin/bash

source export_vars.sh

$pacman -S awesome-terminal-fonts \
           capitaine-cursors \
           perl-font-ttf \
           terminus-font \
           ttf-anonymous-pro \
           ttf-dejavu \
           ttf-fira-mono \
           ttf-fira-sans \
           ttf-hack \
           ttf-liberation \
           ttf-ubuntu-font-family

$yay -S gruvbox-dark-gtk \
        gruvbox-dark-icons-gtk \
        gruvbox-icon-theme \
        nerd-fonts-anonymous-pro \
        nerd-fonts-complete-mono-glyphs \
        nerd-fonts-dejavu-complete \
        otf-nerd-fonts-fira-code \
        ttf-nerd-fonts-hack-complete-git
