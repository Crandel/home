# ZSH SPECIFIC
# zmodload zsh/zprof
# start=`date +%s.%N`
# THE FOLLOWING LINES WERE ADDED BY COMPINSTALL
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*' menu select=2
zstyle ':completion:*' menu select=interactive
zstyle :compinstall filename '$HOME/.zshrc'

# END OF LINES ADDED BY COMPINSTALL
setopt AUTOCD EXTENDEDGLOB NOTIFY PROMPT_SUBST
setopt AUTO_NAME_DIRS CORRECTALL MAGIC_EQUAL_SUBST
bindkey -e
unsetopt nomatch # escape string fixing zsh: no matches found error
autoload -Uz compinit; compinit
SPROMPT='Correct %B%F{red}%U%R%b%f%u to %F{green}%r%f? [%By%bes|%BN%bo|%Be%bdit|%Ba%bbort] '
# END ZSH SPECIFIC

# HISTORY
setopt HIST_IGNORE_ALL_DUPS SHARE_HISTORY INC_APPEND_HISTORY EXTENDED_HISTORY
setopt HIST_IGNORE_SPACE HIST_FIND_NO_DUPS HIST_SAVE_NO_DUPS
HISTFILE=$HOME/.hist_zsh
HISTSIZE=5000000
SAVEHIST=$HISTSIZE
# END HISTORY

# LOCAL FUNCTIONS
# Arch Linux command-not-found support, you must have package pkgfile installed
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && source /usr/share/doc/pkgfile/command-not-found.zsh

# NAVIGATION
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

autoload edit-command-line; zle -N edit-command-line
bindkey '^h' edit-command-line

typeset -g -A key

key[Home]="${terminfo[khome]}"
key[End]="${terminfo[kend]}"
key[Insert]="${terminfo[kich1]}"
key[Backspace]="${terminfo[kbs]}"
key[Delete]="${terminfo[kdch1]}"
key[Up]="${terminfo[kcuu1]}"
key[Down]="${terminfo[kcud1]}"
key[Left]="${terminfo[kcub1]}"
key[Right]="${terminfo[kcuf1]}"
key[PageUp]="${terminfo[kpp]}"
key[PageDown]="${terminfo[knp]}"
key[Shift-Tab]="${terminfo[kcbt]}"

# setup key accordingly
[[ -n "${key[Home]}"      ]] && bindkey -- "${key[Home]}"       beginning-of-line
[[ -n "${key[End]}"       ]] && bindkey -- "${key[End]}"        end-of-line
[[ -n "${key[Insert]}"    ]] && bindkey -- "${key[Insert]}"     overwrite-mode
[[ -n "${key[Backspace]}" ]] && bindkey -- "${key[Backspace]}"  backward-delete-char
[[ -n "${key[Delete]}"    ]] && bindkey -- "${key[Delete]}"     delete-char
[[ -n "${key[Up]}"        ]] && bindkey -- "${key[Up]}"         up-line-or-beginning-search
[[ -n "${key[Down]}"      ]] && bindkey -- "${key[Down]}"       down-line-or-beginning-search
[[ -n "${key[Left]}"      ]] && bindkey -- "${key[Left]}"       backward-char
[[ -n "${key[Right]}"     ]] && bindkey -- "${key[Right]}"      forward-char
[[ -n "${key[PageUp]}"    ]] && bindkey -- "${key[PageUp]}"     beginning-of-buffer-or-history
[[ -n "${key[PageDown]}"  ]] && bindkey -- "${key[PageDown]}"   end-of-buffer-or-history
[[ -n "${key[Shift-Tab]}" ]] && bindkey -- "${key[Shift-Tab]}"  reverse-menu-complete

# \e[A arrow up
# \e[B arrow down
# \e[C arrow right
# \e[D arrow left
# bindkey "\e[A" up-line-or-beginning-search
# bindkey "^[0A" up-line-or-beginning-search
# bindkey "\e[B" down-line-or-beginning-search
# bindkey "^[0B" down-line-or-beginning-search

# \e[1;5A Ctrl + arrow up
# \e[1;5B Ctrl + arrow down
# \e[1;5C Ctrl + arrow right
# \e[1;5D Ctrl + arrow left
# should be binded after zsh-users/zsh-history-substring-search loading
bindkey "\e[1;5A" history-substring-search-up
bindkey "\e[1;5B" history-substring-search-down
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word

# \e[1;2A Shift + arrow up
# \e[1;2B Shift + arrow down
# \e[1;2C Shift + arrow right
# \e[1;2D Shift + arrow left
bindkey "\e[1;2A" history-incremental-search-forward
bindkey "\e[1;2B" history-incremental-search-backward # Ctrl+r
bindkey "\e[1;2C" end-of-line
bindkey "\e[1;2D" beginning-of-line

# fix of delete key
bindkey "\e[3~" delete-char
bindkey "\e[3;5~" delete-word
# fix for separating text on slashes
export WORDCHARS='*?[]~&;!#$%^(){}<>'

# NAVIGATION END

if test -t 1; then
  # see if it supports colors...
  force_color_prompt=yes
  color_prompt=yes
  export TERM="xterm-256color"
  export LS_COLORS=$LS_COLORS:"di=01;35"

  # enable color support of ls and also add handy aliases
  alias ls='ls --color=auto'
  alias less='less -R'
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# ALIASES
export PERS_DIR='/data/work'
alias less="less --LONG-PROMPT --no-init --quit-at-eof --quit-if-one-screen --quit-on-intr"
export PAGER='less -SRXF'

alias arch='uname -m'
alias ll='ls -ahlF --time-style=long-iso --group-directories-first'
alias la='ls -A'
alias home_pr='cd $PERS_DIR/home'
alias compress_jpeg="find ./ -iname '*.jpg' -or -iname '*.jpeg' -type f -size +100k -exec jpeg-recompress --quality high --method ssim --accurate --min 70 {} {} \;"
alias compress_png="find ./ -iname '*.png' -type f -size +100k -exec optipng {} \;"
alias -g G='|grep'
alias -g L='|less'
alias check_adb='adb devices -l'


# CUSTOM FUNCTIONS
command_exists () {
  (( $+commands[$1] ))
}

# SUDO
SUDO=''
if [[ $EUID -ne 0 ]] && command_exists sudo ; then
  SUDO='sudo'
fi
# END SUDO

# COMPLETIONS
if command_exists jira
then
  eval "$(jira --completion-script-zsh)"
fi
# END COMPLETIONS

project_folders="$PERS_DIR/projects"
prj () {
  cd $project_folders
  if [ ! -z $1 ]; then
    cd $1
  fi
}
compdef "_path_files -W $project_folders -/ && return 0 || return 1" prj

backup_dir="$PERS_DIR/backup"
backup () {
  cd $backup_dir
  if [ ! -z $1 ]; then
    cd $1
  fi
}
compdef "_path_files -W $backup_dir -/ && return 0 || return 1" backup

## SWAP
soff () {
  eval "$SUDO swapoff $(swapon --noheadings --show=NAME)"
}

son () {
  eval "$SUDO swapon /swapfile"
}
## END SWAP

## ARCHIVES
extract () {
  if [ -f $1 ] ; then
    case $1 in
      *.tar.bz2)        tar xjf $1        ;;
      *.tar.gz)         tar xzf $1        ;;
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

zipin () {
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

return_root (){
  xhost si:localuser:root
}

mkcd() {
  folder=$@
  mkdir -p $folder
  cd $folder
}
# END CUSTOM FUNCTIONS

# IMPORT ADDITIONAL FILES
## CUSTOM FUNCS
if [ -f ~/.func.zsh ]; then
  fpath+=~/.func.zsh
  . ~/.func.zsh
fi

## CUSTOM ALIASES AND EXPORTS
if [ -f ~/.aliases.zsh ]; then
  . ~/.aliases.zsh
fi

## USER SPECIFIC BINARIES
LOCAL_BIN=$HOME/.local/bin
if [ -d $LOCAL_BIN ]; then
  export PATH=$PATH:$LOCAL_BIN
fi

if [ -f /etc/profile.d/vte.sh ]; then
  . /etc/profile.d/vte.sh
fi
# END IMPORT

# PLUGIN MANAGMENT
plugin_init() {
  source $zinit_source
  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  zinit wait lucid light-mode for \
          OMZL::clipboard.zsh \
          djui/alias-tips \
          zsh-users/zsh-completions \
          zsh-users/zsh-history-substring-search \
        atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
          zdharma/fast-syntax-highlighting \
        blockf \
          zsh-users/zsh-completions \
        atload"!_zsh_autosuggest_start" \
          zsh-users/zsh-autosuggestions as"completion" \
        has'cargo' \
          OMZP::cargo/_cargo \
        has'docker' \
          OMZP::docker/_docker \
        has'docker-compose' \
          OMZP::docker-compose \
        has'git' \
          OMZP::git \
        has'kubectl' \
          OMZP::kubectl \
        has'minikube' \
          OMZP::minikube \
        has'pip' \
          OMZP::pip \
        has'sbt' \
          OMZP::sbt

  zinit lucid light-mode for \
        OMZP::shrink-path

  zinit lucid load for \
        has'poetry' \
        load'[[ $(ls) = *pyproject.toml* ]]' \
          darvid/zsh-poetry

  compinit
}

zinit_source="$HOME/.zinit/bin/zinit.zsh"
if [ ! -f $zinit_source ]; then
  mkdir ~/.zinit
  git clone https://github.com/zdharma/zinit.git ~/.zinit/bin
fi
plugin_init
unfunction plugin_init

# PACKAGE MANAGERS
if command_exists pacman ; then
  paclist() {
    pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'
  }

  alias p="$SUDO pacman"
  alias pql='pacman -Ql'
  alias pqs='pacman -Qs'
  alias pas='pacman -Ss'
  alias upg='p -Syu'
  alias pii='pacman -Sii'
  pai() {
    $SUDO pacman -S --needed $@
    rehash
  }
  alias par='p -Rs'
  if command_exists yay ; then
    alias yay='yay --aur --editmenu --builddir $PERS_DIR/bb'
    alias upy='yay -Syua'
    alias yss='yay -Ss'
    alias yai='yay -Sa'
    alias yii='yay -Sii'
  fi

  recovery-pacman() {
    pacman "$@"  \
         --log /dev/null   \
         --noscriptlet     \
         --dbonly          \
         --force           \
         --nodeps          \
         --needed
  }
fi

if command_exists apt ; then
  alias a="$SUDO apt"
  alias pas='apt search'
  alias upd="a update"
  alias upy='a upgrade'
  alias upl='apt list --upgradable'
  alias upg='upd && sleep 2 && upl && sleep 2 && upy'
  alias pai='a install'
  alias par='a remove'
  alias pql="dpkg-query -L"
  alias aar="$SUDO add-apt-repository"
fi

if command_exists yum ; then
  alias y="$SUDO yum"
  alias upg='yum upgrade'
  alias pas='yum search'
  alias pai='yum install'
  alias par='yum remove'
fi
# END PACKAGE MANAGERS

# SYSTEM TOOLS
## VMs
### VAGRANT
if command_exists vagrant ; then
  alias vup='vagrant up'
  alias vh='vagrant halt'
  alias vsus='vagrant suspend'
  alias vre='vagrant reload'
  alias vs='vagrant ssh'
fi

### DOCKER
if command_exists docker ; then
  alias d='docker'
  compdef d='docker'
  alias dc='docker-compose'
  compdef dc='docker-compose'
  alias dl='docker-compose logs --tail 15'
  alias drun='docker-compose stop && docker-compose run --service-ports'
  alias dst='d stop $(d ps -q)'
  alias drm='d rm $(d ps -aq)'
  alias dvrm='d volume rm $(d volume ls -q)'
  drmin () {
    for img in $(d images | rg -i 'none' | awk '{print $3}'); do
      docker rmi $img
    done
  }
  d_exec() {
    docker exec -it $1 sh -c "stty cols $COLUMNS rows $LINES && sh -l";
  }
  export d_exec;
  d_ip() {
    doc_ip=$(ip a show docker0 | grep "inet " | awk '{split($2, a, "/"); print a[1]}')
    export DOCKER_HOST_IP=$doc_ip
  }
  export d_ip
fi

### KUBERNETES
if command_exists kubectl ; then
  alias k='kubectl'
  alias kapr='kubectl api-resources'
  if command_exists kubectx ; then
    alias ktx='kubectx'
  fi
  if command_exists kubens ; then
    alias kns='kubens'
  fi
  # alias k9s_cont_namesp='k9s --context <context> -n <namespace>'
  # pod_pf() {
  #   context=""
  #   namespace=""
  #   pod=$(kubectl get pod -l name=<pod name> \
  #                         --context $context                     \
  #                         -n $namespace                          \
  #                         -o jsonpath="{.items[0].metadata.name}")
  #   echo "pod is $pod"
  #   kubectl port-forward $pod --context $context -n $namespace 8000:8000
  # }
fi

if command_exists aws ; then
  alias aelogin='aws ecr get-login --region eu-central-1'
  if command_exists saml2aws ; then
    export SAML2AWS_SESSION_DURATION=36000
    alias sl='saml2aws login -a default -p default -r eu-central-1 --skip-prompt'
  fi
fi
## END VM's

## MEDIA TOOLS
if command_exists youtube-dl ; then
  alias ytdl='youtube-dl'
  alias ytb='ytdl -f "bestvideo[height<=1080]"+bestaudio' # --external-downloader aria2c --external-downloader-args "-x 10 -s 10"'
  alias ytm='ytdl -f bestaudio -x'
  alias ytlf='ytdl --list-formats'
fi

if command_exists aria2c ; then
  alias a2c='aria2c -x 10 -s 10'
fi

if command_exists ffplay ; then
  alias play='ffplay -nodisp -autoexit'
fi
## END MEDIA TOOLS

## EDITORS
if command_exists vim; then
  alias v='vim'
  alias sv="$SUDO vim"
  export EDITOR='vim'
fi
if command_exists emacs ; then
  alias em='emacs -nw'
  alias e='emacs -nw'
  alias sem="$SUDO emacs -nw"
  export EDITOR='editor-run'
fi
## END EDITORS

## FILE MANAGERS
if command_exists mc ; then
  alias smc="$SUDO mc"
fi

if command_exists vifm ; then
  alias vf="vifm"
  alias svf="$SUDO vifm"
  if [ -n "$INSIDE_VIFM" ]; then
    INSIDE_VIFM="[V]"
  fi
fi

if command_exists nnn ; then
  alias nnn='nnn -d'
  export NNN_USE_EDITOR=1
  export NNN_CONTEXT_COLORS='2745'
  export NNN_COPIER=$(which xsel)
  export NNN_NOTE=/opt/work/backup/notes
  export NNN_OPS_PROG=1
fi
## END FILE MANAGERS

if command_exists systemctl ; then
  alias ssystemctl="$SUDO systemctl"
fi

if command_exists git ; then
  alias g='git'
  compdef g=git
  alias pla='g pull'
  alias pll='pla origin'
  alias psh='g push origin'
  alias gst='g status'
  alias gco='g checkout'
  alias gadd='g add'
  alias gcmt='g commit -m'
fi

if command_exists tmux ; then
  alias tm='tmux attach || tmux new'
fi

if command_exists qt5ct ; then
  export QT_QPA_PLATFORMTHEME="qt5ct"
  export QT_PLATFORM_PLUGIN="qt5ct"
fi

if command_exists clipmenud ; then
  export CM_LAUNCHER=bemenu
  export CM_DIR=$HOME/.cache/.clipmenud
fi

if command_exists bemenu ; then
  export BEMENU_OPTS='-I 0 -i --fn "Hack:26" --nb "#1e1e1e" --nf "#c0f440" --sf "#1e1e1e" --sb "#f4800d" --tb "#d7dd90" --tf "#111206" --hb "#49088c" --hf "#c2fbd3"'
fi

if command_exists reflector ; then
  alias gen_mirror='reflector --ipv4 --country Germany --age 12 -p https -l 10 --sort score --save /tmp/mirrorlist'
  alias gen_rsync='reflector --ipv4 --country Germany --age 12 -p rsync -l 10 --sort score --save /tmp/powerpill'
fi
# END SYSTEM TOOLS

# PROGRAMM LANGUAGES
## RUST

if command_exists cargo || [ -d $HOME/.rustup ]; then
  if [ ! -d $HOME/.cargo/bin ]; then
    mkdir -p $HOME/.cargo/bin
  fi
  export PATH=$PATH:$HOME/.cargo/bin
  alias crn='cargo run'
  alias cup='cargo update'
  alias cbd='cargo build'
  alias cbr='cargo build --release'
  if ! command_exists cargo-expand; then
    cargo install cargo-expand
  fi
  if ! command_exists cargo-audit; then
    cargo install cargo-audit
  fi
  if ! command_exists cargo-outdated; then
    cargo install cargo-outdated
  fi
fi

if [ -d /usr/src/rust ]; then
  export RUST_SRC_PATH=/usr/src/rust/src
fi

if command_exists bat ; then
  alias ct='bat'
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi

if ! command_exists tldr ; then
  echo "install tealdeer"
fi

if ! command_exists rg ; then
  echo "install ripgrep"
else
  export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
fi

if command_exists sk ; then
  alias fzf='sk'
  if [[ -d /usr/share/skim/completion.zsh ]]; then
    source /usr/share/skim/completion.zsh
  fi
  gdelbrs() {
    git branch |
      rg --invert-match '\*' |
      cut -c 3- |
      sk --multi --preview="git log {} --" |
      xargs --no-run-if-empty git branch --delete --force
  }
else
  echo "install sk"
fi

if command_exists zoxide; then
  eval "$(zoxide init --no-aliases zsh)"
  alias j='__zoxide_z' # cd to highest ranked directory matching path
  alias ja='__zoxide_za' # add path to the database
  alias ji='__zoxide_zi' # cd with interactive selection using fzf
  alias jr='__zoxide_zr' # remove path from the database
else
  echo "Please install zoxide"
fi

if command_exists lsd; then
  alias ls='lsd'
  alias ll='ls -ahlF --group-dirs=first'
fi
## END RUST

## GO
go16_path=/usr/lib/go-1.16/bin
if [ -d $go16_path ]; then
  export PATH=$PATH:$go16_path
fi

if command_exists go ; then
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOPATH/bin
  if [ -d $GOPATH/goprojects ]; then
    export GOPATH=$GOPATH:$GOPATH/goprojects
  fi
fi

if command_exists fzf ; then
  gdelbrf() {
    git branch |
      rg --invert-match '\*' |
      cut -c 3- |
      fzf --multi --preview="git log {} --" |
      xargs --no-run-if-empty git branch --delete --force
  }
else
  echo "install fzf"
fi
if command_exists lf ; then
  lfcd () {
      tmp="$(mktemp)"
      lf -last-dir-path="$tmp" "$@"
      if [ -f "$tmp" ]; then
          dir="$(cat "$tmp")"
          rm -f "$tmp"
          [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
      fi
  }
  bindkey -s '^o' 'lfcd\n'
fi
## END GO

## SCALA
if [ -d /usr/share/scala ]; then
  export SCALA_HOME=/usr/share/scala
  export PATH=$PATH:$SCALA_HOME/bin
fi

if command_exists scala ; then
  alias s='scala'
  alias sc='scalac'
  srun() {
    name=$@
    echo "Start compilation for $name"
    scalac "$name.scala"
    echo "Compilation done"
    scala $name
  }
  if command_exists coursier; then
    alias openapi-generator-cli="coursier launch org.openapitools:openapi-generator-cli:latest.release --"
  fi
fi
## END SCALA

## PYTHON
if [ -d "$HOME/.pyenv" ]; then
   export PYENV_ROOT="$HOME/.pyenv"
   export PATH="$PYENV_ROOT/bin:$PATH"
   export PYENV_VIRTUALENV_DISABLE_PROMPT=1
   eval "$(pyenv init --path)"
   eval "$(pyenv init - zsh)"
   eval "$(pyenv virtualenv-init - zsh)"
fi

clean_pyc (){
  find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}
### Determine active Python virtualenv details.
set_virtualenv () {
  if [ ! -z "$PYENV_VIRTUAL_ENV" ] ; then
    echo " %F{yellow}[$(pyenv version-name)]%f"
  fi
}
## END PYTHON

## NPM
if command_exists npm; then
  NPM_PACKAGES="${HOME}/.config/npm-packages"
  mkdir -p $NPM_PACKAGES
  export NODE_PATH="$NPM_PACKAGES/lib/node_modules:$NODE_PATH"
  export PATH=$PATH:$NPM_PACKAGES/bin
fi

# END PROGRAMM LANGUAGES

# PROMPT
function parse_git_branch(){
  git branch 2> /dev/null | sed -n 's/^\* //p'
}

## Determine the branch/state information for this git repository.
function set_git_branch() {
  # Get the final branch string.
  if command_exists git_status ; then
    branch="$(git_status zsh)"
  else
    branch="$(parse_git_branch)"
  fi

  if [ -n "${branch}" ]; then
    echo " ($branch)"
  fi
}

function set_prompt_symbol () {
  echo "%(?.%F{yellow}.%F{red}[%?])$INSIDE_VIFM"
  echo "╰─➤%f"
}

fish_pwd() {
  if typeset -f shrink_path > /dev/null; then
    echo "$(shrink_path -f)"
  else
    echo "%~"
  fi
}

# Set the prompt.
set_zsh_prompt () {
  [[ $SSH_CONNECTION ]] && local uath='%F{white}@%M%f'
  PROMPT='%F{yellow}╭─%B%T%b%f$(set_virtualenv) %(!.%F{red}.%F{green})%n%f${uath} %F{magenta}{$(fish_pwd)}%f$(set_git_branch) $(set_prompt_symbol) '
}

# end=`date +%s.%N`
# printf "%.2f" $((end-start))
# Tell zsh to execute this function just before displaying its prompt.
set_zsh_prompt

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  # Possible arguments sway|i3|xfce
  exec ~/.local/bin/initrc sway
fi
