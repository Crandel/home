#!/usr/bin/bash

source export_vars.sh

$pacman -S bzip2 \
           gzip \
           i7z \
           p7zip \
           tar \
           zip \
           unrar

$yay -S archivemount \
        rar2fs
