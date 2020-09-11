#!/usr/bin/bash

source export_vars.sh

$pacman -S fzf \
           mc \
           ripgrep \
           the_silver_searcher \
           vifm \
           zsh \
           zsh-completions \
           zsh-doc

$yay -S tealdeer \
        zoxide
