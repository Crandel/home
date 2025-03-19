#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "libreoffice-still"
  "meld"
  "nano"
  "rust-analyzer"
  "bash-language-server"
  "lua-language-server"
  "typescript-language-server"
  "yaml-language-server"
)

install_pacman $packages

declare -a y_pkgs=(
  "emacs-pgtk-git"
  "kotlin-language-server"
  "sql-language-server"
  "vim-language-server"
)

install_yay $y_pkgs
