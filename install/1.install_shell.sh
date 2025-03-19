#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "arch-wiki-docs"
  "bat"
  "chezmoi"
  "fzf"
  "jq"
  "htop"
  "lsd"
  "mc"
  "navi"
  "ripgrep"
  "tealdeer"
  "vim"
  "vifm"
  "xh"
  "zoxide"
  "zsh"
  "zsh-doc"
)

install_pacman $packages
