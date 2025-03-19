#!/usr/bin/env bash

source export_vars.sh

declare -a packages=(
  "catdoc"
  "curlftpfs"
  "docx2txt"
  "gthumb"
  "fuse-zip"
  "fuseiso"
  "fzf"
  "highlight"
  "mp3info"
  "pygmentize"
  "sshfs"
  "zathura"
  "zathura-djvu"
  "zathura-pdf-poppler"
)

install_pacman $packages

declare -a aur_packages=(
  "amberol"
  "archivemount"
  "epub2txt"
  "python-pdftotext"
  "rar2fs"
  "viu"
)


install_yay $aur_packages
