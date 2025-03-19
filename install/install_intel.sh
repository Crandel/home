#!/usr/bin/bash

source export_vars.sh

$pacman -S vulkan-intel \
           intel-ucode \
           lib32-vulkan-intel \
           intel-media-driver \
           lib32-libva-intel-driver
