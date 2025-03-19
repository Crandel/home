AUR_PACKAGE_DIR=/data/linux/bb
mkdir -p $AUR_PACKAGE_DIR
export yay="yay --aur --editmenu --builddir $AUR_PACKAGE_DIR"


function package_exists () {
  pacman -Qk $1 > /dev/null
}

if [[ $EUID -ne 0 ]] && package_exists sudo; then
  export pacman="sudo pacman"
fi

if ! package_exists yay ; then
  $pacman -S git
  git clone https://aur.archlinux.org/yay.git /tmp/yay
  cd /tmp/yay
  makepkg -si
fi

function install_pacman () {
  packages=$1
  exs_pgs=""
  for pkg in "${packages[@]}"
  do
    if ! package_exists $pkg ; then
      echo "Package $pkg doesn't exists"
      exs_pgs+="$pkg "
    fi
  done

  if [ ! -z "$exs_pgs" ]; then
    echo "Install $exs_pgs"
    $pacman -S -y $exs_pgs
  fi
}

function install_yay () {
  y_pkgs=$1
  exs_ypgs=""
  for ypkg in "${y_pkgs[@]}"
  do
    if ! package_exists $ypkg ; then
      echo "Package $ypkg doesn't exists"
      exs_ypgs+="$ypkg "
    fi
  done

  if [ ! -z "$exs_ypgs" ]; then
    echo "Install non existing packages $exs_ypgs"
    $yay -S -y  $exs_ypgs
  fi
}

