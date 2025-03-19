#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "delve"
  "go"
  "go-task"
  "go-tools"
  "gopls"
  "graphviz"
)

install_pacman $packages

declare -a y_pkgs=(
  "go-impl"
  "godoctor-git"
  "gotest"
  "gotests"
  "golangci-lint"
  "gomodifytags"
  "gore"
)

install_yay $y_pkgs
