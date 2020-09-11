#!/usr/bin/bash

source export_vars.sh

$pacman -S fzf \
           mc \
           vifm \
           the_silver_searcher \
           zsh \
           zsh-completions \
           zsh-doc

$yay -S zoxide
