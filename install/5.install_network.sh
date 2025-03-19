#!/usr/bin/bash

source export_vars.sh

declare -a packages=(
  "blueman"
  "bluez"
  "bluez-plugins"
  "bluez-tools"
  "bluez-utils"
  "ethtool"
  "inetutils"
  "iwd"
  "mkinitcpio-netconf"
  "net-tools"
  "nfs-utils"
  "nmap"
  "wifite"
)

install_pacman $packages

declare -a y_pkgs=(
  "iwgtk"
)

install_yay $y_pkgs
