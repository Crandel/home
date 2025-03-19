#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "jre11-openjdk"
  "openjdk11-doc"
  "openjdk11-src"
  "sbt"
  "scala"
  "scala-docs"
  "scala-sources"
)

install_pacman $packages

declare -a y_pkgs=(
  "intellij-idea-community-edition-no-jre"
  "coursier"
)

install_yay $y_pkgs
