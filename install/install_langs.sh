#!/usr/bin/bash

source export_vars.sh

$pacman -S bpython \
          helm \
          jre11-openjdk \
          kubectl \
          kubectx \
          openjdk11-doc \
          openjdk11-src \
          rustup \
          sbt \
          scala \
          scala-docs \
          scala-sources

$yay -S coursier \
        k9s \
        litecli \
        pgcli
