#!/usr/bin/bash

source export_vars.sh

$pacman -S dav1d \
           deadbeef \
           fbv \
           ffmpeg \
           ffmpegthumbnailer \
           gst-libav \
           imagemagick \
           jbig2dec \
           lame \
           leptonica \
           libjpeg-turbo \
           libva-intel-driver \
           lib32-libpulse \
           mp3info \
           mpv \
           nomacs \
           opus \
           opusfile \
           pipewire \
           pipewire-jack \
           pipewire-pulse \
           png++ \
           pulseaudio \
           pulseaudio-bluetooth \
           pulseaudio-jack \
           pulsemixer \
           rav1e \
           vlc \
           xdg-desktop-portal \
           xdg-desktop-portal-gtk \
           xdg-desktop-portal-kde \
           zathura \
           zathura-djvu \
           zathura-pdf-mupdf

$yay -S xdg-desktop-portal-wlr
