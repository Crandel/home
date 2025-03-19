#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "aspell-de"
  "aspell-en"
  "aspell-ru"
  "aspell-uk"
  "hunspell-de"
  "hunspell-en_US"
)

install_pacman $packages

declare -a y_pkgs=(
  "hunspell-ru"
  "hunspell-uk"
)

install_yay $y_pkgs
