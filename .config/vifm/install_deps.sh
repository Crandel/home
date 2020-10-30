#! /bin/bash

pacman_packages=(
catdoc
curlftpfs
docx2txt
feh
fuse-zip
fuseiso
fzf
highlight
mp3info
pygmentize
sshfs
zathura
zathura-djvu
zathura-pdf-poppler
)

aur_packages=(
archivemount
epub2txt
python-pdftotext
rar2fs
viu
)

for package in ${pacman_packages[@]}; do
  sudo pacman -S --noconfirm $package
  sleep 1
done

for apackage in ${aur_packages[@]}; do
  yay -S --noconfirm --aur --editmenu --builddir /data/work/bb $apackage
  sleep 1
done
