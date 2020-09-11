mkdir -p /opt/work/bb
export yay='yay --aur --editmenu --builddir /data/work/bb'

if [[ $EUID -ne 0 ]] && command -v "sudo"  > /dev/null 2>&1; then
  export pacman="sudo pacman"
fi


function command_exists () {
  command -v "$1"  > /dev/null 2>&1;
}

if ! command_exists yay ; then
  $pacman -S git
  git clone https://aur.archlinux.org/yay.git /tmp/yay
  cd /tmp/yay
  makepkg -si
fi
