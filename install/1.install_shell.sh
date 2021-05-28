#!/usr/bin/bash

source export_vars.sh

$pacman -S \
        fzf \
        lsd \
        mc \
        ripgrep \
        skim \
        the_silver_searcher \
        vifm \
        xh \
        zsh \
        zsh-completions \
        zsh-doc

$yay -S \
     navi \
     tealdeer \
     zoxide
