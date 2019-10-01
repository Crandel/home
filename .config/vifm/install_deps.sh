#! /bin/bash

pacman_packages=(fzf zathura zathura-djvu zathura-pdf-poppler highlight feh docx2txt catdoc curlftpfs sshfs fuseiso)

aur_packages=(rar2fs archivemount)

for package in ${pacman_packages[@]}; do
  sudo pacman -S --noconfirm $package
  sleep 1
done

for apackage in ${aur_packages[@]}; do
  yay -S --noconfirm $apackage
  sleep 1
done
