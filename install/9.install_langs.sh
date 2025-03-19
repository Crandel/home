#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "bpython"
  "k9s"
  "kubectl"
  "kubectx"
  "rust"
  "staticcheck"
)

install_pacman $packages

declare -a y_pkgs=(
  "litecli"
  "mycli"
  "pgcli"
)

install_yay $y_pkgs
