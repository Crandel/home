#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "celluloid"
  "dav1d"
  "ffmpeg"
  "ffmpegthumbnailer"
  "flac"
  "geoip-database"
  "gst-libav"
  "gst-plugin-libcamera"
  "gst-plugin-pipewire"
  "gstreamer-vaapi"
  "imagemagick"
  "jbig2dec"
  "lame"
  "lib32-gst-plugins-base"
  "lib32-gst-plugins-base-libs"
  "lib32-gst-plugins-good"
  "lib32-gstreamer"
  "libjpeg-turbo"
  "libva-utils"
  "mp3info"
  "mpc"
  "mpd"
  "mpv"
  "opus"
  "opusfile"
  "pipewire"
  "pipewire-audio"
  "pipewire-jack"
  "pipewire-pulse"
  "pipewire-v4l2"
  "png++"
  "pulsemixer"
  "rav1e"
  "vlc"
  "wireplumber"
  "xdg-desktop-portal"
  "xdg-desktop-portal-gtk"
  "xdg-desktop-portal-wlr"
  "zathura"
  "zathura-djvu"
  "zathura-pdf-mupdf"
)
install_pacman $packages

declare -a y_pkgs=(
  "mpdris2"
  "subliminal"
  "papers"
)

install_yay $y_pkgs
