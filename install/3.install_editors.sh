#!/usr/bin/bash

source export_vars.sh

$pacman -S bash-language-server \
           # code \
           emacs \
           libreoffice-still \
           meld \
           nano \
           rust-analyzer \
           python-language-server \
           vim

$yay -S metals \
        intellij-idea-community-edition-no-jre \
        vim-language-server \
        vscode-html-languageserver-bin \
        yaml-language-server-bin
