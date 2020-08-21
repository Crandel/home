#!/usr/bin/bash

source export_vars.sh

$pacman -S ethtool \
           inetutils \
           net-tools \
           networkmanager \
           networkmanager-openvpn \
           nfs-utils \
           nm-connection-editor \
           nmap \
           mkinitcpio-netconf \
           wifite \
           wireguard-dkms \
           wireguard-tools
