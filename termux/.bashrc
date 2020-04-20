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
alias ll='ls -ahlF --group-directories-first'
alias la='ls -A'
alias L='|less'
alias G='|grep'
alias ~='cd $HOME'

# fix for git
alias meld='vimdiff'

if command_exists bat ; then
  alias ct='bat'
fi

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

con_jpg_pdf (){
  convert *.jpg $@.pdf
}

clean_pyc (){
  find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
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
    alias yay='yay --aur --editmenu --builddir $PERS_DIR/bb'
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

if command_exists git; then
  alias g='git'
  alias pla="g pull"
  alias pll="pla origin"
  alias psh="g push origin"
  alias gst="g status"
  alias gco="g checkout"
  alias gadd="g add"
  alias gcmt="g commit -m"
fi

if command_exists tmux ; then
  alias tm='tmux attach || tmux new'
fi

if command_exists go ; then
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOPATH/bin
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
fi

# Rust
if [ -d $HOME/.cargo/bin ]; then
  export PATH=$PATH:$HOME/.cargo/bin
fi

if command_exists cargo ; then
  if ! command_exists rg ; then
    cargo install ripgrep
  fi
fi

if [ -d /usr/src/rust ]; then
  export RUST_SRC_PATH=/usr/src/rust/src
fi
# End Rust

if command_exists emacs; then
  alias em='emacs -nw'
  alias sem="$SUDO emacs -nw"
  export EDITOR='emacs -nw'
elif command_exists vim; then
  export EDITOR='vim'
fi

# file managers
if command_exists mc; then
  alias smc="$SUDO mc"
fi

if command_exists vifm; then
  alias svf="$SUDO vifm"
  alias vf="vifm"
fi

if command_exists nnn ; then
  alias nnn='nnn -d'
  export NNN_USE_EDITOR=1
  export NNN_CONTEXT_COLORS='2745'
  export NNN_COPIER=$(which xsel)
  export NNN_NOTE=/opt/work/backup/notes
  export NNN_OPS_PROG=1
fi
#end file managers

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

if [ -f ~/.aliases.bash ]; then
  . ~/.aliases.bash
fi

LOCAL_BIN=$HOME/.local/bin
if [ -d $LOCAL_BIN ]; then
  export PATH=$PATH:$LOCAL_BIN
fi

# PROMPT
# get current status of git repo
function parse_git_branch(){
  branch=`git branch 2> /dev/null | sed -n 's/^\* //p'`
  if [ ! "${branch}" == "" ]; then
    staged_files=''
    unstaged_files=''
    new_status=`git status --porcelain`
    ahead=`git status -sb 2> /dev/null | grep -o "ahead [0-9]*" | grep -o "[0-9]*"`
    behind=`git status -sb 2> /dev/null | grep -o "behind [0-9]*" | grep -o "[0-9]*"`
    # staged files
    X=`echo -n "${new_status}" 2> /dev/null | cut -c 1-1`
    # unstaged files
    Y=`echo -n "${new_status}" 2> /dev/null | cut -c 2-2`
    modified_unstaged=`echo -n "${Y}" | grep "M" -c`
    deleted_unstaged=`echo -n "${Y}" | grep "D" -c`
    untracked_unstaged=`echo -n "${Y}" | grep "?" -c`
    modified_staged=`echo -n "${X}" | grep "M" -c`
    deleted_staged=`echo -n "${X}" | grep "D" -c`
    renamed_staged=`echo -n "${X}" | grep "R" -c`
    new_staged=`echo -n "${X}" | grep "A" -c`
    # unstaged_files
    if [ "${modified_unstaged}" != "0" ]; then
      unstaged_files="%${modified_unstaged}${unstaged_files}"
    fi
    if [ "${deleted_unstaged}" != "0" ]; then
      unstaged_files="-${deleted_unstaged}${unstaged_files}"
    fi
    if [ "${untracked_unstaged}" != "0" ]; then
      unstaged_files="*${untracked_unstaged}${unstaged_files}"
    fi
    # staged_files
    if [ "${modified_staged}" != "0" ]; then
      staged_files="%${modified_staged}${staged_files}"
    fi
    if [ "${deleted_staged}" != "0" ]; then
      staged_files="-${deleted_staged}${staged_files}"
    fi
    if [ "${renamed_staged}" != "0" ]; then
      staged_files="^${renamed_staged}${staged_files}"
    fi
    if [ "${new_staged}" != "0" ]; then
      staged_files="+${new_staged}${staged_files}"
    fi
    if [ ! "${staged_files}" == "" ]; then
      staged_files="|${GREEN}${staged_files}${NORMAL}"
    fi
    if [ ! "${unstaged_files}" == "" ]; then
      unstaged_files="|${YELLOW}${unstaged_files}${NORMAL}"
    fi
    if [ ! "${ahead}" == "" ]; then
      ahead="${LIGHT_GREEN}{>${ahead}}${NORMAL}"
    fi
    if [ ! "${behind}" == "" ]; then
      behind="${LIGHT_RED}{<${behind}}${NORMAL}"
    fi
    # Set the final branch string.
    echo "${branch}${ahead}${behind}${unstaged_files}${staged_files}"
  fi
}

# Determine the branch/state information for this git repository.
function set_git_branch() {
  BRANCH=''
  # Get the name of the branch.
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

  # Set the BRANCH variable.
  set_git_branch

  # history -a
  # history -c
  # history -r
  if [[ $EUID -eq 0 ]] ; then
    USERCOLOR="${RED}"
  fi

  # Set the bash prompt variable.
  PS1="${NEW_LINE} ${BLUE}\A${NORMAL} ${USERCOLOR}\u${NORMAL} ${PURPLE}{\w}${NORMAL}${BRANCH}${P_SYMBOL}"
}

# Tell bash to execute this function just before displaying its prompt.
export PROMPT_COMMAND=set_bash_prompt
