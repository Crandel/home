mkdir -p /opt/work/bb
export yay='yay --aur --editmenu --builddir /opt/work/bb'

if [[ $EUID -ne 0 ]] && command -v "sudo"  > /dev/null 2>&1; then
  export pacman="sudo pacman"
fi
