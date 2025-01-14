project_folders="$PERS_DIR/projects"
prj() {
  cd $project_folders
  if [ ! -z $1 ]; then
    cd $1
  fi
}
_prj() {
  _path_files -/ -W $project_folders && return 0 || return 1
}
compdef _prj prj

export BACKUP_DIR="$PERS_DIR/backup"
bcp() {
  cd $BACKUP_DIR
  if [ ! -z $1 ]; then
    cd $1
  fi
}
_backup() {
  _path_files -W $BACKUP_DIR -/ && return 0 || return 1
}
compdef _backup bcp

cf() {
  cd $HOME/.config
  if [ ! -z $1 ]; then
    cd $1
  fi
}
_config_dir() {
  _path_files -W $HOME/.config -/ && return 0 || return 1
}
compdef _config_dir cf

## SWAP
soff() {
  eval "$SUDO swapoff $(swapon --noheadings --show=NAME)"
}

son() {
  eval "$SUDO swapon $(swapon --noheadings --show=NAME)"
}
## END SWAP

## ARCHIVES
extract() {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)        tar xjf $1        ;;
      *.tar.gz)         tar xzf $1        ;;
      *.tar.xz)         tar xf  $1        ;;
      *.bz2)            bunzip2 $1        ;;
      *.rar)            unrar x $1        ;;
      *.gz)             gunzip $1         ;;
      *.tar)            tar xf $1         ;;
      *.tbz2)           tar xjf $1        ;;
      *.tgz)            tar xzf $1        ;;
      *.zip)            unzip $1          ;;
      *.Z)              uncompress $1     ;;
      *.7z)             7zr e $1          ;;
      *)                echo "'$1' cannot be extracted via extract()" ;;
    esac
  else
    echo "'$1' is not a valid file"
  fi
}

zipin() {
  for f in $(ls -A);
    do
    if [ -f "$f" ]; then
      case $f in
        *.zip)       echo "$f already zipped"  ;;
        *)           zip -9 $f.zip $f && rm $f ;;
      esac
    fi;
  done
}
## END ARCHIVES

file_replace() {
  for file in $(find . -type f -name "$1*"); do
    mv $file $(echo "$file" | sed "s/$1/$2/");
  done
}

# convert jpg files to single pdf
con_jpg_pdf (){
  convert *.jpg $@.pdf
}

# convert png files to single pdf
con_png_pdf (){
  convert *.png $@.pdf
}

# send requests periodically
send_requests() {
    local url=$1
    local interval=$2
    local count=$3

    for i in $(seq 1 $count); do
        curl -s "$url"
        echo
        sleep $interval
    done
}
# Add music to specified playlist
add_music (){
  music_dir="$1"
  playlist="$2"
  # Loop through all mp3 files (adjust for your format)
  for file in "$music_dir"/*; do
    mpc addplaylist "$playlist" "$file"
  done

  echo "Songs added to playlist!"
}
## X11 VS WAYLAND
disable_x11 (){
  systemctl --user disable clipmenud.service
  systemctl --user disable kbdd.service
  systemctl --user disable dunst.service
}

enable_x11 (){
  systemctl --user enable clipmenud.service
  systemctl --user enable kbdd.service
  systemctl --user enable dunst.service
}
## END X11 VS WAYLAND

## TOGGLE HDMI SOUND
hdmi_sound_on (){
  pactl --server "unix:$XDG_RUNTIME_DIR/pulse/native" set-card-profile 0 output:hdmi-stereo+input:analog-stereo
}
hdmi_sound_off (){
  pactl --server "unix:$XDG_RUNTIME_DIR/pulse/native" set-card-profile 0 output:analog-stereo+input:analog-stereo
}

return_root() {
  xhost si:localuser:root
}

mkcd() {
  folder=$@
  mkdir -p $folder
  cd $folder
}

# Mount usb device
mdev() {
  dev=$1
  $SUDO mount /dev/$dev $HOME/mnt
}

# Unmount usb device
umdev() {
  $SUDO umount -R $HOME/mnt
}

if [ -n "${WAYLAND_DISPLAY:-}" ] && (( ${+commands[wl-copy]} )) && (( ${+commands[wl-paste]} )); then
  clipcopy() {
    wl-copy < "${1:-/dev/stdin}"
  }
  clippaste() {
    wl-paste
  }
fi

}
## Lang specific functions

paclist() {
  pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'
}

recovery-pacman() {
  pacman "$@"  \
         --log /dev/null   \
         --noscriptlet     \
         --dbonly          \
         --force           \
         --nodeps          \
         --needed
}

function git_main_branch() {
  command git rev-parse --git-dir &>/dev/null || return
  local ref
  for ref in refs/{heads,remotes/{origin,upstream}}/{main,trunk}; do
    if command git show-ref -q --verify $ref; then
      echo ${ref:t}
      return
    fi
  done
  echo master
}

function git_current_branch() {
  local ref
  ref=$(__git_prompt_git symbolic-ref --quiet HEAD 2> /dev/null)
  local ret=$?
  if [[ $ret != 0 ]]; then
    [[ $ret == 128 ]] && return  # no git repo.
    ref=$(__git_prompt_git rev-parse --short HEAD 2> /dev/null) || return
  fi
  echo ${ref#refs/heads/}
}

clean_pyc (){
  find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}
drmin() {
  for img in $(d images | rg -i 'none' | awk '{print $3}'); do
    docker rmi $img
  done
}
d_exec() {
  docker exec -it $1 sh -c "stty cols $COLUMNS rows $LINES && sh -l";
}
d_ip() {
  doc_ip=$(ip a show docker0 | grep "inet " | awk '{split($2, a, "/"); print a[1]}')
  export DOCKER_HOST_IP=$doc_ip
}

## Start Rust
setup_cargo() {
  rustup completions zsh cargo  > $LOCAL_ZSH_COMP_DIR/_cargo
  rustup completions zsh rustup > $LOCAL_ZSH_COMP_DIR/_rustup
}
setup_cargo_tools() {
  if ! command_exists cargo-expand; then
    cargo install cargo-expand
  fi
  if ! command_exists cargo-audit; then
    cargo install cargo-audit
  fi
  if ! command_exists cargo-outdated; then
    cargo install cargo-outdated
  fi
}
gdelbrs() {
  git branch |
  rg --invert-match '\*' |
  cut -c 3- |
  sk --multi --preview="git log {} --" |
  xargs --no-run-if-empty git branch --delete --force
}
## End Rust

## GO
gdelbrf() {
  git branch |
    rg --invert-match '\*' |
    cut -c 3- |
    fzf --multi --preview="git log {} --" |
    xargs --no-run-if-empty git branch --delete --force
}
## END GO

# End lang specific functions

# Custom host functions

if [ -f $ZDOTDIR/func.local.zsh ]; then
  . $ZDOTDIR/func.local.zsh
fi
