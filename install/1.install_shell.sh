#!/usr/bin/bash

source export_vars.sh

$pacman -S \
        fzf \
        lsd \
        mc \
        ripgrep \
        the_silver_searcher \
        vifm \
        zsh \
        zsh-completions \
        zsh-doc

$yay -S \
     navi \
     tealdeer \
     zoxide
