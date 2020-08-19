#!/usr/bin/bash

source export_vars.sh

$pacman -S vulkan-intel \
           intel-ucode \
           lib32-vulkan-intel \
           libva-intel-driver \
           xf86-video-intel \
           lib32-libva-intel-driver \
           libva1-intel-driver
