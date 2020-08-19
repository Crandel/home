#!/usr/bin/bash

source export_vars.sh

$pacman -S alacritty \
           aria2 \
           autoconf \
           automake \
           autopep8 \
           bat \
           bind \
           binutils \
           cups \
           cups-filters \
           cups-pdf \
           curlftpfs \
           dconf-editor \
           gscan2pdf \
           gucharmap \
           gvfs \
           gvfs-mtp \
           gvfs-nfs \
           highlight \
           hq \
           htop \
           jq \
           keepassxc \
           make \
           man-db \
           man-pages \
           mc \
           mtpfs \
           ncdu \
           openssh \
           openvpn \
           pacmanlogviewer \
           patch \
           pavucontrol \
           qt5ct \
           quiterss \
           rclone \
           reflector \
           ripgrep \
           source-highlight \
           tig \
           tilix \
           tmux \
           traceroute \
           tree \
           vifm \
           wget \
           wpa_supplicant \
           youtube-dl \
           yq

yay -S android-bash-completion \
       openjpeg \
       tealdeer
