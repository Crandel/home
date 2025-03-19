#!/usr/bin/bash

source export_vars.sh

  # "aria2"
declare -a packages=(
  "alacritty"
  "binutils"
  "cups"
  "cups-filters"
  "cups-pdf"
  "curlftpfs"
  "cpupower"
  "dconf-editor"
  "docker"
  "docker-compose"
  "github-cli"
  "git-delta"
  "gscan2pdf"
  "gucharmap"
  "gvfs"
  "gvfs-mtp"
  "gvfs-nfs"
  "highlight"
  "hq"
  "keepassxc"
  "make"
  "man-db"
  "man-pages"
  "mc"
  "mtpfs"
  "ncdu"
  "ollama"
  "openssh"
  "openvpn"
  "pacmanlogviewer"
  "patch"
  "pavucontrol"
  "rclone"
  "reflector"
  "source-highlight"
  "sshd"
  "tig"
  "thunar"
  "tmux"
  "traceroute"
  "tree"
  "wezterm"
  "wget"
  "yt-dlp"
  "yq"
  "upower"
)

install_pacman $packages

declare -a y_pkgs=(
  "openjpeg"
  "light"
  "pacseek"
)

install_yay $y_pkgs
