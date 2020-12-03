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

# CUSTOM FUNCTIONS
function command_exists () {
  (( $+commands[$1] ))
}

# NAVIGATION
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# \e[A arrow up
# \e[B arrow down
# \e[C arrow right
# \e[D arrow left
bindkey "\e[A" up-line-or-beginning-search
bindkey "\e[B" down-line-or-beginning-search

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
alias arch='uname -m'
alias ll='ls -ahlF --time-style=long-iso --group-directories-first'
alias la='ls -A'
alias home_pr='cd $PERS_DIR/home'
alias less="less --LONG-PROMPT --no-init --quit-at-eof --quit-if-one-screen --quit-on-intr"
alias compress_jpeg="find ./ -iname '*.jpg' -type f -size +100k -exec jpeg-recompress --quality high --method ssim --accurate --min 70 {} {} \;"
alias -g G='|grep'
alias -g L='|less'
export PAGER='less -SRXF'
export PERS_DIR='/data/work'

# SUDO
SUDO=''
if [[ $EUID -ne 0 ]] && command_exists sudo ; then
  SUDO='sudo'
fi
# END SUDO

# Bash completions
if command_exists jira
then
  eval "$(jira --completion-script-zsh)"
fi
# END Bash completions

project_folders="$PERS_DIR/projects"
function prj () {
  cd $project_folders
  if [ ! -z $1 ]; then
    cd $1
  fi
}
compdef "_path_files -W $project_folders -/ && return 0 || return 1" prj

backup_dir="$PERS_DIR/backup"
function backup () {
  cd $backup_dir
  if [ ! -z $1 ]; then
    cd $1
  fi
}
compdef "_path_files -W $backup_dir -/ && return 0 || return 1" backup

## SWAP
function soff {
  eval "$SUDO swapoff $(swapon --noheadings --show=NAME)"
}

function son {
  eval "$SUDO swapon $(swapon --noheadings --show=NAME)" #/dev/mapper/xubuntu--vg-swap_1
}
## END SWAP

## ARCHIVES
function extract () {
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

function file_replace() {
  for file in $(find . -type f -name "$1*"); do
    mv $file $(echo "$file" | sed "s/$1/$2/");
  done
}

# convert jpg files to single pdf
con_jpg_pdf (){
  convert *.jpg $@.pdf
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
# END CUSTOM FUNCTIONS

# IMPORT ADDITIONAL FILES
## CUSTOM ZSH FUNCS
if [ -f ~/.zfunc.zsh ]; then
  fpath+=~/.zfunc.zsh
  . ~/.zfunc.zsh
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
function plugin_init() {
  source $zinit_source
  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  zinit wait lucid light-mode for \
          MichaelAquilina/zsh-autoswitch-virtualenv \
          OMZL::clipboard.zsh \
          OMZP::colored-man-pages \
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
        has'pip' \
          OMZP::pip \
        has'sbt' \
          OMZP::sbt

  zinit lucid light-mode for \
        OMZP::shrink-path \
        load'[[ $PWD = */inf* ]]' unload'[[ $PWD != */inf* ]]'  \
        has'kubectl' \
          OMZP::kube-ps1 \
        load'[[ $PWD = */terraform* ]]' unload'[[ $PWD != */terraform* ]]' \
        has'terraform' \
          OMZP::terraform \
        load'[[ $PWD = */helm* ]]' unload'[[ $PWD != */helm* ]]' \
        has'helm' \
          OMZP::helm

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

  alias pacman="$SUDO pacman"
  alias upg='pacman -Syu'
  alias pacs='pacman -Ss'
  alias pqs='pacman -Qs'
  alias pql='pacman -Ql'
  alias pqi='pacman -Sii'
  alias paci='pacman -S --needed'
  alias pacr='pacman -Rs'
  if command_exists yay ; then
    alias yay='yay --aur --editmenu --builddir $PERS_DIR/bb'
    alias upgy='yay -Syua'
    alias yacs='yay -Ss'
    alias yaci='yay -Sa'
  fi
  if command_exists powerpill ; then
    alias upg="$SUDO powerpill -Syu"
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
  alias apt="$SUDO apt"
  alias upd='apt update'
  alias upgy='apt upgrade'
  alias upl='apt list --upgradable'
  alias upg='upd && sleep 2 && upl && sleep 2 && upgy'
  alias pacs='apt search'
  alias paci='apt install'
  alias pacr='apt remove'
  alias pql="dpkg-query -L"
  alias aar="$SUDO add-apt-repository"
fi

if command_exists yum ; then
  alias apt="$SUDO yum"
  alias upg='yum upgrade'
  alias pacs='yum search'
  alias paci='yum install'
  alias pacr='yum remove'
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
  alias dl='docker-compose logs --tail 15'
  alias run='docker-compose stop && docker-compose run --service-ports'
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
fi

### KUBERNETES
if command_exists kubectl ; then
  alias k='kubectl'
  alias kapr='kubectl api-resources'
  compdef k='kubectl'
  if command_exists kubectx ; then
    alias ktx='kubectx'
    compdef ktx='kubectx'
  fi
  if command_exists kubens ; then
    alias kns='kubens'
    compdef kns='kubens'
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
  alias ytb='youtube-dl -f "bestvideo[height<=1080]"+bestaudio' # --external-downloader aria2c --external-downloader-args "-x 10 -s 10"'
  alias ytm='youtube-dl -f bestaudio -x'
fi

if command_exists aria2c ; then
  alias a2c='aria2c -x 10 -s 10'
fi

if command_exists ffplay ; then
  alias play='ffplay -nodisp -autoexit'
fi
## END MEDIA TOOLS

## EDITORS
if command_exists emacs ; then
  alias em='emacs -nw'
  alias sem="$SUDO emacs -nw"
  export EDITOR='editor-run'
elif command_exists vim; then
  export EDITOR='vim'
fi
## END EDITORS

## FILE MANAGERS
if command_exists mc ; then
  alias smc="$SUDO mc"
fi

if command_exists vifm ; then
  alias vf="vifm"
  alias svf="$SUDO vifm"
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

if command_exists bat ; then
  alias ct='bat'
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi

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
fi

if command_exists clipmenud ; then
  export CM_LAUNCHER=bemenu
  export CM_DIR=$HOME/.cache/.clipmenud
fi

if command_exists bemenu ; then
  export BEMENU_OPTS='-I 0 -i -m 0 --fn "Hack:26" --nb "#1e1e1e" --nf "#c0f440" --sf "#1e1e1e" --sb "#f4800d" --tb "#d7dd90" --tf "#111206" --hb "#49088c" --hf "#c2fbd3"'
fi

if command_exists reflector ; then
  alias gen_mirror='reflector --ipv4 -p https -f 10 --sort rate --save /tmp/mirror'
  alias gen_rsync='reflector --ipv4 -p rsync -f 10 --sort rate --save /tmp/powerpill'
fi
# END SYSTEM TOOLS

# PROGRAMM LANGUAGES
## GO
if command_exists go ; then
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOPATH/bin
  if [ -d $GOPATH/goprojects ]; then
    export GOPATH=$GOPATH:$GOPATH/goprojects
  fi

  if ! command_exists fzf ; then
    go get -u github.com/junegunn/fzf
    if [ -f ~/go/src/github.com/junegunn/fzf/shell/key-bindings.zsh ]; then
      . ~/go/src/github.com/junegunn/fzf/shell/key-bindings.zsh
    fi
  fi
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

## PROLOG
if command_exists swipl ; then
  swi_path=/usr/lib/swipl
  if [ -d $swi_path ]; then
    export SWI_HOME_DIR=$swi_path
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SWI_HOME_DIR/lib/x86_64-linux
  fi
fi
## END PROLOG

## RUST
if command_exists cargo ; then
  if [ ! -d $HOME/.cargo/bin ]; then
    mkdir -p $HOME/.cargo/bin
  fi
  export PATH=$PATH:$HOME/.cargo/bin
  alias crn='cargo run'
  alias cup='cargo update'
  alias cbd='cargo build'
  alias cbr='cargo build --release'
  if ! command_exists tldr ; then
    cargo install tealdeer
  fi
  if ! command_exists rg ; then
    cargo install ripgrep
  fi
fi

if [ -d /usr/src/rust ]; then
  export RUST_SRC_PATH=/usr/src/rust/src
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
## END RUST

## PYTHON
virtual='virtualenvwrapper.sh'
if command_exists $virtual; then
  export VIRTUAL_ENV_DISABLE_PROMPT=1
  export WORKON_HOME=~/.virtualenvs/
  export AUTOSWITCH_SILENT=1
  source $virtual
fi

clean_pyc (){
  find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}
### Determine active Python virtualenv details.
function set_virtualenv () {
  if [ ! -z "$VIRTUAL_ENV" ] ; then
    echo " %F{yellow}[`basename \"$VIRTUAL_ENV\"`]"
  fi
}
## END PYTHON
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
  kps=""
  if typeset -f kube_ps1 > /dev/null; then
    kps=$(kube_ps1)
  fi
  echo "$kps %(?.%F{yellow}.%F{red}[%?])"
  echo "╰─➤%f"
}

function fish_pwd() {
  if typeset -f shrink_path > /dev/null; then
    echo "$(shrink_path -f)"
  else
    echo "%~"
  fi
}

# Set the prompt.
function set_zsh_prompt () {
  PROMPT='%F{yellow}╭─%B%T%b%f$(set_virtualenv) %(!.%F{red}.%F{green})%n%f %F{magenta}{$(fish_pwd)}%f$(set_git_branch) $(set_prompt_symbol) '
}

# end=`date +%s.%N`
# printf "%.2f" $((end-start))
# Tell zsh to execute this function just before displaying its prompt.
set_zsh_prompt

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  # Possible arguments sway|i3|xfce
  exec ~/.local/bin/initrc sway
fi
