#!/usr/bin/env bash

source export_vars.sh

declare -a packages=(
  "tree-sitter"
  "tree-sitter-bash"
  "tree-sitter-c"
  "tree-sitter-javascript"
  "tree-sitter-lua"
  "tree-sitter-markdown"
  "tree-sitter-python"
  "tree-sitter-rust"
)
install_pacman $packages

declare -a y_pkgs=(
  "tree-sitter-cpp"
  "tree-sitter-go"
  "tree-sitter-gomod-git"
  "tree-sitter-html-git"
  "tree-sitter-json"
  "tree-sitter-sql"
  "tree-sitter-toml"
  "tree-sitter-typescript-git"
  "tree-sitter-yaml-git"
)

install_yay $y_pkgs
