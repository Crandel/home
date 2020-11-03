#!/usr/bin/bash

source export_vars.sh

$pacman -S \
        bpython \
        flake8 \
        helm \
        jre11-openjdk \
        kubectl \
        kubectx \
        openjdk11-doc \
        openjdk11-src \
        python-rope \
        rust \
        sbt \
        scala \
        scala-docs \
        scala-sources \
        yarf

$yay -S \
     coursier \
     k9s \
     litecli \
     mycli \
     pgcli \
     tfswitch
