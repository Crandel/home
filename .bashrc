# ~/.bashrc: executed by bash(1) for non-login shells.

function command_exists () {
  command -v "$1"  > /dev/null 2>&1;
}

# COLORS
RED=""
YELLOW=""
GREEN=""
BLUE=""
CYAN=""
PURPLE=""
LIGHT_RED=""
LIGHT_GREEN=""
WHITE=""
LIGHT_GRAY=""
NORMAL=""
# check if stdout is a terminal...
if test -t 1; then
  # see if it supports colors...
  force_color_prompt=yes
  color_prompt=yes
  export TERM="xterm-256color"
  RED="\[\033[0;31m\]"
  YELLOW="\[\033[1;33m\]"
  GREEN="\[\033[0;32m\]"
  BLUE="\[\033[1;34m\]"
  PURPLE="\[\033[0;35m\]"
  CYAN="\[\033[0;36m\]"
  LIGHT_RED="\[\033[1;31m\]"
  LIGHT_GREEN="\[\033[1;32m\]"
  WHITE="\[\033[1;37m\]"
  LIGHT_GRAY="\[\033[0;37m\]"
  NORMAL="\[\e[0m\]"
  # enable color support of ls and also add handy aliases
  bind 'set colored-completion-prefix on'
  bind 'set colored-stats on'
  alias ls='ls --color=auto'
  alias less='less -R'
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
  # NAVIGATION
  bind '"\e[1;5C":forward-word'
  bind '"\e[1;5D":backward-word'
  # bind '"\eOD":backward-word'
  # bind '"\eOC":forward-word'
  # bind '"\eOA":history-search-backward'
  # bind '"\eOB":history-search-forward'
  bind '"\e[A":history-search-backward'
  bind '"\e[B":history-search-forward'

  bind 'set completion-ignore-case on'
  bind 'set show-all-if-ambiguous on'
  bind 'set completion-query-items 30'
  bind 'set editing-mode emacs'
fi

# HISTORY
# don't put duplicate lines or lines starting with space in the history.
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTCONTROL=ignoreboth
HISTSIZE=
HISTFILESIZE=
HISTFILE=$HOME/.hist_bash
HISTTIMEFORMAT="%F %T "

# append to the history file, don't overwrite it
shopt -s histappend
shopt -s checkwinsize
shopt -s cdspell
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"


# ALIASES
# some more ls aliases
alias arch='uname -m'
alias ll='ls -ahlF'
alias la='ls -A'
alias L='|less'
alias G='|grep'
alias ~='cd $HOME'

if [ -f ~/.bash_aliases ]; then
  . ~/.bash_aliases
fi

# CUSTOM FUNCTIONS
project_folders="/opt/work/projects/"
function prj () {
  cd $project_folders
  if [ ! -z $1 ]; then
    cd $1
  fi
}
_prj()
{
    local cur=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$(ls $project_folders)" -- $cur) )
}
complete -F _prj prj

backup_dir="/opt/work/backup/"
function backup () {
  cd $backup_dir
  if [ ! -z $1 ]; then
    cd $1
  fi
}
_backup()
{
    local cur=${COMP_WORDS[COMP_CWORD]}
    COMPREPLY=( $(compgen -W "$(ls $backup_dir)" -- $cur) )
}
complete -F _backup backup

function soff {
  eval "$SUDO swapoff $(swapon --noheadings --show=NAME)"
}

function son {
  eval "$SUDO swapon $(swapon --noheadings --show=NAME)" #/dev/mapper/xubuntu--vg-swap_1
}

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

# BINS CONDITIONS
SUDO=''
if [[ $EUID -ne 0 ]] && command_exists sudo ; then
  complete -cf sudo
  SUDO='sudo'
fi

if command_exists pacman ; then
  alias pacman="$SUDO pacman"
  alias upg='pacman -Syu'
  alias pacs='pacman -Ss'
  alias pqs='pacman -Qs'
  alias pql='pacman -Ql $1'
  alias paci='pacman -S --needed'
  alias pacr='pacman -Rs'
  if command_exists yay ; then
    alias yay='yay --aur --builddir $PERS_DIR/bb'
    alias upgy='yay -Syua'
    alias yacs='yay -Ss'
    alias yaci='yay -Sa'
  fi
  if command_exists powerpill ; then
    alias upg="$SUDO powerpill -Syu"
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

if command_exists apt ; then
  alias apt="$SUDO apt"
  alias upgy='apt update'
  alias upg='upgy && apt upgrade'
  alias pacs='apt search'
  alias paci='apt install'
  alias pacr='apt remove'
  alias pql="dpkg-query -L"
  alias aar="$SUDO add-apt-repository"
  if command_exists apt-fast ; then
    alias upgy='apt-fast update'
    alias upg='apt-fast update && apt-fast upgrade'
    alias paci='apt-fast install'
  fi
fi

if command_exists yum ; then
  alias apt="$SUDO yum"
  alias upg='yum upgrade'
  alias pacs='yum search'
  alias paci='yum install'
  alias pacr='yum remove'
fi

if command_exists tmux ; then
  alias tm='tmux attach || tmux new'
fi

if command_exists docker ; then
  # Docker
  alias d='docker'
  alias dc='docker-compose'
  alias dl='docker-compose logs --tail 15'
  alias run='docker-compose stop && docker-compose run --rm --service-ports app'
  alias dst='d stop $(d ps -q)'
  alias drm='d rm $(d ps -aq)'
fi

if command_exists vagrant ; then
  # Vagrant
  alias vup='vagrant up'
  alias vh='vagrant halt'
  alias vsus='vagrant suspend'
  alias vre='vagrant reload'
  alias vs='vagrant ssh'
fi

if command_exists systemctl ; then
  alias systemctl="$SUDO systemctl"
fi


virtual='virtualenvwrapper.sh'
if command_exists $virtual && [ -z "$VIRTUAL_ENV_DISABLE_PROMPT" ]; then
  export VIRTUAL_ENV_DISABLE_PROMPT=1
  export WORKON_HOME=~/.virtualenvs/
  . $virtual
fi

if command_exists go ; then
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOPATH/bin
fi

if [ -d /usr/share/scala ]; then
  export SCALA_HOME=/usr/share/scala
  export PATH=$PATH:$SCALA_HOME/bin
fi

# Rust
if [ -d $HOME/.cargo/bin ]; then
    export PATH=$PATH:$HOME/.cargo/bin
fi

if [ -d /usr/src/rust ]; then
    export RUST_SRC_PATH=/usr/src/rust/src
fi
# End Rust

if [ -d /usr/lib/jvm/default ]; then
  export JAVA_HOME=/usr/lib/jvm/default
elif [ -d /usr/lib/jvm/default-java ]; then
  export JAVA_HOME=/usr/lib/jvm/default-java
fi

if command_exists emacs; then
  alias em='emacs -nw'
  alias sem="$SUDO emacs -nw"
  export EDITOR='emacs -nw'
elif command_exists vim; then
  export EDITOR='vim'
fi

if command_exists mc; then
  alias smc="$SUDO mc"
fi

if command_exists git; then
  alias pll="git pull origin"
  alias psh="git push origin"
  alias gst="git status"
  alias gco="git checkout"
  alias gadd="git add ."
  alias gcmt="git commit -m"
fi

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi

LOCAL_BIN=$HOME/.local/bin
if [ -d $LOCAL_BIN ]; then
  export PATH=$PATH:$LOCAL_BIN
fi

# PROMPT
# get current status of git repo
function parse_git_branch(){
  git branch 2> /dev/null | sed -n 's/^\* //p'
}
# Determine the branch/state information for this git repository.
function set_git_branch() {
  # Get the name of the branch.
  BRANCH=""
  if command_exists git_status ; then
    branch="$(git_status bash)"
  else
    branch="$(parse_git_branch)"
  fi

  if [ ! "${branch}" == "" ]; then
    BRANCH=" (${CYAN}$branch${NORMAL})"
  fi
}

# Return the prompt symbol to use, colorized based on the return value of the
# previous command.
function set_prompt_symbol () {
  if test $1 -eq 0 ; then
    P_SYMBOL="${BLUE}\n➤${NORMAL} "
  else
    P_SYMBOL="${LIGHT_RED}[$1]\n➤${NORMAL} "
  fi
}


# Determine active Python virtualenv details.
function set_virtualenv () {
  PYTHON_VIRTUALENV=""
  if ! [[ -z ${VIRTUAL_ENV_DISABLE_PROMPT} ]] && [ -f .venv ] ; then
      workon `cat .venv`
  fi

  if ! test -z "$VIRTUAL_ENV" ; then
    PYTHON_VIRTUALENV=" ${YELLOW}[`basename \"$VIRTUAL_ENV\"`]${NORMAL}"
  fi
}

function new_line () {
  NEW_LINE=""
  echo -en "\033[6n" > /dev/tty && read -sdR CURPOS
  if [[ ${CURPOS##*;} -gt 1 ]]; then
      NEW_LINE="${RED}¬\n${NORMAL}"
  fi
}

# Set the full bash prompt.
function set_bash_prompt () {
  local EXIT_CODE="$?"
  local USERCOLOR="${GREEN}"
  # Set the P_SYMBOL variable. We do this first so we don't lose the
  # return value of the last command.
  new_line

  set_prompt_symbol $EXIT_CODE
  # Set the PYTHON_VIRTUALENV variable.
  set_virtualenv

  # Set the BRANCH variable.
  set_git_branch

  # history -a
  # history -c
  # history -r
  if [[ $EUID -eq 0 ]] ; then
    USERCOLOR="${RED}"
  fi

  # Set the bash prompt variable.
  PS1="${NEW_LINE} ${BLUE}\A${NORMAL}${PYTHON_VIRTUALENV} ${USERCOLOR}\u${NORMAL}@${WHITE}\h${NORMAL} ${PURPLE}{\w}${NORMAL}${BRANCH}${P_SYMBOL}"
}

# Tell bash to execute this function just before displaying its prompt.
export PROMPT_COMMAND=set_bash_prompt
