#! /bin/bash

pacman_packages=(fzf zathura zathura-djvu zathura-pdf-poppler highlight dox2txt catdoc curlftpfs sshfs fuseiso)

aur_packages=(rar2fs archivemount)

for package in ${pacman_packages[@]}; do
  echo "$package"; # pacman -S $package
done

for apackage in ${aur_packages[@]}; do
  echo "$apackage"; # yay -S $apackage
done
