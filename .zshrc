# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*' menu select=2
zstyle ':completion:*' menu select=interactive
zstyle :compinstall filename '$HOME/.zshrc'

# custon zsh funcs
fpath+=~/.zfunc

autoload -Uz compinit && compinit
# End of lines added by compinstall

# ZSH SPECIFIC
setopt AUTOCD EXTENDEDGLOB NOTIFY PROMPT_SUBST MAGIC_EQUAL_SUBST AUTO_NAME_DIRS CORRECTALL
bindkey -e
autoload -Uz promptinit && promptinit

export PERS_DIR='/opt/work'
export PAGER='less -SRXF'
virtual='virtualenvwrapper.sh'
if (( $+commands[$virtual] )); then
  export VIRTUAL_ENV_DISABLE_PROMPT=1
  export WORKON_HOME=~/.virtualenvs/
  export AUTOSWITCH_SILENT=1
  source $virtual
fi

# FUNCTIONS
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

function soff {
  eval "$SUDO swapoff $(swapon --noheadings --show=NAME)"
}

function son {
  eval "$SUDO swapon $(swapon --noheadings --show=NAME)" #/dev/mapper/xubuntu--vg-swap_1
}

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

con_jpg_pdf (){
  convert *.jpg $@.pdf
}

clean_pyc (){
  find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}
# ANTIGEN

antigen_source="$HOME/antigen.zsh"

function anti_init() {
  . $antigen_source
  antigen use oh-my-zsh
  antigen bundle git
  antigen bundle pip
  antigen bundle command-not-found
  antigen bundle shrink-path
  antigen bundle kubectl
  antigen bundle mvn
  antigen bundle sbt
  antigen bundle scala
  antigen bundle cargo
  antigen bundle docker-compose
  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle zsh-users/zsh-completions
  antigen bundle zsh-users/zsh-history-substring-search
  antigen bundle zsh-users/zsh-syntax-highlighting
  antigen bundle MichaelAquilina/zsh-autoswitch-virtualenv
  antigen apply
}

if [ -f $antigen_source ]; then
  anti_init
else;
  curl -L git.io/antigen > $antigen_source
  anti_init
fi

# Lines configured by zsh-newuser-install

# HISTORY
setopt HIST_IGNORE_ALL_DUPS SHARE_HISTORY INC_APPEND_HISTORY EXTENDED_HISTORY HIST_IGNORE_SPACE HIST_FIND_NO_DUPS HIST_SAVE_NO_DUPS
HISTFILE=~/.hist_zsh
HISTSIZE=5000000
SAVEHIST=$HISTSIZE
# History end

# End of lines configured by zsh-newuser-install
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

if test -t 1; then
  force_color_prompt=yes
  color_prompt=yes
  export TERM="xterm-256color"
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
alias ~='cd $HOME'
alias home_pr='cd $PERS_DIR/home'
alias less="less --LONG-PROMPT --no-init --quit-at-eof --quit-if-one-screen --quit-on-intr"
alias compress_jpeg="find ./ -iname '*.jpg' -type f -size +100k -exec jpeg-recompress --quality high --method ssim --accurate --min 70 {} {} \;"

if (( $+commands[bat] )) ; then
  alias ct='bat'
fi
alias G='|grep'
alias L='|less'

# NAVIGATION
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

[[ -n "$key[Up]"   ]] && bindkey -- "$key[Up]"   up-line-or-beginning-search
[[ -n "$key[Down]" ]] && bindkey -- "$key[Down]" down-line-or-beginning-search
bindkey "^[[1;5A" history-substring-search-up
bindkey "^[[1;5B" history-substring-search-down

bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word

# bindkey "\eOC" forward-word
# bindkey "\eOD" backward-word

# bindkey "\e[A" history-search-backward
# bindkey "\e[B" history-search-forward

# bindkey "\eOA" history-search-backward
# bindkey "\eOB" history-search-forward


# CONDITIONS
SUDO=''
if [[ $EUID -ne 0 ]] && (( $+commands[sudo] )) ; then
  SUDO='sudo'
fi

# Package managers
if (( $+commands[pacman] )) ; then
  paclist() {
  # Source: https://bbs.archlinux.org/viewtopic.php?id=93683
  LC_ALL=C pacman -Qei $(pacman -Qu | cut -d " " -f 1) | \
    awk 'BEGIN {FS=":"} /^Name/{printf("\033[1;36m%s\033[1;37m", $2)} /^Description/{print $2}'
  }

  alias pacman="$SUDO pacman"
  alias upg='pacman -Syu'
  alias pacs='pacman -Ss'
  alias pqs='pacman -Qs'
  alias pql='pacman -Ql $1'
  alias paci='pacman -S --needed'
  alias pacr='pacman -Rs'
  if (( $+commands[yay] )) ; then
    alias yay='yay --aur --editmenu --builddir $PERS_DIR/bb'
    alias upgy='yay -Syua'
    alias yacs='yay -Ss'
    alias yaci='yay -Sa'
  fi
  if (( $+commands[powerpill] )) ; then
    alias upg='$SUDO powerpill -Syu'
  fi
  recovery-pacman() {
    sudo pacman "$@"  \
    --log /dev/null   \
    --noscriptlet     \
    --dbonly          \
    --force           \
    --nodeps          \
    --needed
  }
fi

if (( $+commands[apt] )) ; then
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

if (( $+commands[yum] )) ; then
  alias apt="$SUDO yum"
  alias upg='yum upgrade'
  alias pacs='yum search'
  alias paci='yum install'
  alias pacr='yum remove'
fi
# End Package managers

# Program langs
if (( $+commands[git] )); then
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

if (( $+commands[tmux] )) ; then
  alias tm='tmux attach || tmux new'
fi

  # VMs
    # Vagrant
if (( $+commands[vagrant] )) ; then
  alias vup='vagrant up'
  alias vh='vagrant halt'
  alias vsus='vagrant suspend'
  alias vre='vagrant reload'
  alias vs='vagrant ssh'
fi

    # Docker
if (( $+commands[docker] )) ; then
  alias d='docker'
  compdef d='docker'
  alias dc='docker-compose'
  alias dl='docker-compose logs --tail 15'
  alias run='docker-compose stop && docker-compose run --service-ports'
  alias dst='d stop $(d ps -q)'
  alias drm='d rm $(d ps -aq)'
  alias drmin='d rmi $(d images | rg -i "none" | awk "{print $3}")'
  d_exec(){
    docker exec -it $1 sh -c "stty cols $COLUMNS rows $LINES && sh -l";
  }
  export d_exec;
fi

    # Kubernetes
if (( $+commands[kubectl] )) ; then
  alias kl='kubectl'
  compdef kl='kubectl'
  if (( $+commands[kubectx] )) ; then
    alias ktx='kubectx'
    compdef ktx='kubectx'
  fi
  if (( $+commands[kubens] )) ; then
    alias kns='kubens'
    compdef kns='kubens'
  fi
fi

if (( $+commands[aws] )) ; then
  alias aelogin='aws ecr get-login --region eu-central-1'
  if (( $+commands[saml2aws] )) ; then
    export SAML2AWS_SESSION_DURATION=36000
    alias sl='saml2aws login -a default -p default --skip-prompt'
  fi
fi
  # End VMs

if (( $+commands[systemctl] )) ; then
  alias ssystemctl="$SUDO systemctl"
fi

  # Go
if (( $+commands[go] )) ; then
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOPATH/bin
  if [ -d $GOPATH/goprojects ]; then
    export GOPATH=$GOPATH:$GOPATH/goprojects
  fi

  if (( !$+commands[fzf] )) ; then
    go get -u github.com/junegunn/fzf
    if [ -f ~/go/src/github.com/junegunn/fzf/shell/key-bindings.zsh ]; then
      . ~/go/src/github.com/junegunn/fzf/shell/key-bindings.zsh
    fi
  fi
fi
  # End go

  # Scala
if [ -d /usr/share/scala ]; then
  export SCALA_HOME=/usr/share/scala
  export PATH=$PATH:$SCALA_HOME/bin
fi

if (( $+commands[scala] )); then
  alias s='scala'
  alias sc='scalac'
  srun() {
    name=$@
    echo "Start compilation for $name"
    scalac "$name.scala"
    echo "Compilation done"
    scala $name
  }
fi
  # End Scala

  # Prolog
if (( $+commands[swipl] )); then
  swi_path=/usr/lib/swipl
  if [ -d $swi_path ]; then
    export SWI_HOME_DIR=$swi_path
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$SWI_HOME_DIR/lib/x86_64-linux
  fi
fi
  # End Prolog

  # Rust
if [ -d $HOME/.cargo/bin ]; then
  export PATH=$PATH:$HOME/.cargo/bin
fi

if (( $+commands[cargo] )) ; then
  if (( !$+commands[tldr] )) ; then
    cargo install tealdeer
  fi
  if (( !$+commands[rg] )) ; then
    cargo install ripgrep
  fi
fi

if [ -d /usr/src/rust ]; then
  export RUST_SRC_PATH=/usr/src/rust/src
fi
  # End Rust
# End Program langs

# Media
if (( $+commands[mpv] )) ; then
  alias mpv='mpv --hwdec=vaapi --vo=gpu'
fi

if (( $+commands[youtube-dl] )) ; then
  alias ytb='youtube-dl -f bestvideo+bestaudio'
fi

if (( $+commands[ffplay] )) ; then
  alias play='ffplay -nodisp -autoexit'
fi
# End Media

# Editors
if (( $+commands[emacs] )); then
  alias em='emacs -nw'
  alias sem="$SUDO emacs -nw"
  export EDITOR='emacs'
elif (( $+commands[vim] )); then
  export EDITOR='vim'
fi
# End Editors

# file managers
if (( $+commands[mc] )); then
  alias smc="$SUDO mc"
fi

if (( $+commands[vifm] )); then
  alias vf="vifm"
  alias svf="$SUDO vifm"
fi

if (( $+commands[nnn] )); then
  alias nnn='nnn -d'
  export NNN_USE_EDITOR=1
  export NNN_CONTEXT_COLORS='2745'
  export NNN_COPIER=$(which xsel)
  export NNN_NOTE=/opt/work/backup/notes
  export NNN_OPS_PROG=1
fi
#end file managers

if (( $+commands[qt5ct] )); then
  export QT_QPA_PLATFORMTHEME="qt5ct"
fi

if [ -f ~/.aliases.zsh ]; then
  . ~/.aliases.zsh
fi

LOCAL_BIN=$HOME/.local/bin
if [ -d $LOCAL_BIN ]; then
  export PATH=$PATH:$LOCAL_BIN
fi

if [ -f /etc/profile.d/vte.sh ]; then
  . /etc/profile.d/vte.sh
fi

# PROMPT
function parse_git_branch(){
  git branch 2> /dev/null | sed -n 's/^\* //p'
}

# Determine the branch/state information for this git repository.
function set_git_branch() {
  # Get the final branch string.
  if (( $+commands[git_status] )); then
    branch="$(git_status zsh)"
  else
    branch="$(parse_git_branch)"
  fi

  if [ -n "${branch}" ]; then
    echo " ($branch)"
  fi
}

function set_prompt_symbol () {
  echo " %(?.%F{yellow}.%F{red}[%?])\n➤%f "
}

# Determine active Python virtualenv details.
function set_virtualenv () {
  if [ ! -z "$VIRTUAL_ENV" ] ; then
    echo " %F{yellow}[`basename \"$VIRTUAL_ENV\"`]"
  fi
}

function fish_pwd() {
  if typeset -f shrink_path > /dev/null; then
    echo "$(shrink_path -f)"
  else
    echo "%~"
  fi
}

# Set the full bash prompt.
function set_zsh_prompt () {
  PROMPT=' %F{yellow}%B%T%b%f$(set_virtualenv) %(!.%F{red}.%F{green})%n%f@%F{white}%m%f %F{magenta}{$(fish_pwd)}%f$(set_git_branch)$(set_prompt_symbol)'
}
# Tell zsh to execute this function just before displaying its prompt.
set_zsh_prompt

if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  # export QT_QPA_PLATFORM=wayland
  # export QT_WAYLAND_DISABLE_WINDOWDECORATION="1"
  # export GDK_BACKEND=wayland
  # export GTK_THEME=Adapta-Black-Nokto-Eta-Maia
  # export MOZ_ENABLE_WAYLAND=1
  # export MOZ_DBUS_REMOTE=1
  # export _JAVA_AWT_WM_NONREPARENTING=1
  # exec sway
  exec startx
fi
