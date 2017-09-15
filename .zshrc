# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle :compinstall filename '/home/crandel/.zshrc'

autoload -Uz compinit
compinit
zstyle ':completion:*' menu select=2
# End of lines added by compinstall
# ZSH SPECIFIC
setopt AUTOCD EXTENDEDGLOB NOTIFY PROMPT_SUBST MAGIC_EQUAL_SUBST AUTO_NAME_DIRS CORRECTALL
bindkey -e
autoload -Uz promptinit
promptinit

antigen_source="$HOME/antigen.zsh"
if [ -f $antigen_source ]; then
  source $antigen_source
  antigen use oh-my-zsh
  antigen bundle git
  antigen bundle pip
  antigen bundle command-not-found
  antigen bundle autojump
  antigen bundle shrink-path
  antigen bundle mvn
  antigen bundle sbt
  antigen bundle scala
  antigen bundle cargo
  antigen bundle per-directory-history
  antigen bundle zsh-users/zsh-autosuggestions
  antigen bundle zsh-users/zsh-completions
  antigen bundle zsh-users/zsh-history-substring-search
  antigen bundle zsh-users/zsh-syntax-highlighting
  antigen apply
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
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

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

# ALIASES
alias arch='uname -m'
# some more ls aliases
alias ll='ls -ahlF'
alias la='ls -A'
alias ~='cd $HOME'
alias home_pr='cd $HOME/home/'

# FUNCTIONS
projects_folder="/opt/work/projects/"
function pr () {
  cd $projects_folder
  if [ ! -z $1 ]; then
    cd $1
  fi
}

_pr(){
    local -a list
    for dir in $(ls $projects_folder); do
        inside_dir=$projects_folder$dir
        if [ -d $inside_dir ]; then
            list=( $list $dir )
            for d in $(ls $inside_dir); do
                double_dir=$inside_dir/$d
                if [ -d $double_dir ]; then
                    list=( $list $dir/$d )
                fi
            done
        fi
    done
    compadd -a $list
}

# _p() {
#   _arguments '1: :->first' '2: :->second'
#   case $state in
#     first)
#       _files -W $projects_folder
#     ;;
#     second)
#       _files -W $projects_folder${(Q)words[CURRENT-1]}
#     ;;
#   esac
# }

#compdef '_path_files -W /opt/work/projects -/ && return 0 || return 1' pr
compdef _pr pr

function backup () {
  local backup="/opt/work/backup"
  cd $backup
  if [ ! -z $1 ]; then
    cd $1
  fi
}

function soff {
  eval "$SUDO swapoff $(swapon --noheadings --show=NAME)"
}

function son {
  eval "$SUDO swapon $(swapon --noheadings --show=NAME)" #/dev/mapper/xubuntu--vg-swap_1
}


# CONDITIONS
SUDO=''
if [[ $EUID -ne 0 ]] && (( $+commands[sudo] )) ; then
  SUDO='sudo'
fi

if (( $+commands[pacman] )) ; then
  alias pacman="$SUDO pacman"
  alias upg='pacman -Syu'
  alias upgy='yaourt -Syu'
  alias pacs='pacman -Ss'
  alias paci='pacman -S --needed'
  alias pacr='pacman -Rs'
fi

if (( $+commands[apt] )) ; then
  paclist() {
  # Source: https://bbs.archlinux.org/viewtopic.php?id=93683
  LC_ALL=C pacman -Qei $(pacman -Qu | cut -d " " -f 1) | \
    awk 'BEGIN {FS=":"} /^Name/{printf("\033[1;36m%s\033[1;37m", $2)} /^Description/{print $2}'
  }
  alias apt="$SUDO apt"
  alias upgy='apt update'
  alias upg='upgy && apt upgrade'
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

if (( $+commands[tmux] )) ; then
  alias tm='tmux attach || tmux new'
fi

if (( $+commands[docker] )) ; then
  # Docker
  alias d='docker'
  compdef d='docker'
  alias dc='docker-compose'
  alias dl='docker-compose logs --tail 15'
  alias run='docker-compose stop && docker-compose run --rm --service-ports app'
  alias dst='d stop (d ps -q)'
fi

if (( $+commands[vagrant] )) ; then
  # Vagrant
  alias vup='vagrant up'
  alias vh='vagrant halt'
  alias vsus='vagrant suspend'
  alias vre='vagrant reload'
  alias vs='vagrant ssh'
fi

if (( $+commands[systemctl] )) ; then
  alias systemctl="$SUDO systemctl"
fi

virtual='/usr/bin/virtualenvwrapper.sh'
if [ -f $virtual ]; then
  export VIRTUAL_ENV_DISABLE_PROMPT=1
  export WORKON_HOME=~/.virtualenvs/
  . $virtual
fi

if (( $+commands[go] )) ; then
  export GOPATH=$HOME/go
  export PATH=$PATH:$GOPATH/bin
fi

if (( $+commands[hadoop] )) ; then
  alias hdp='sudo -u hdfs hadoop fs'
fi

if (( $+commands[hive] )) ; then
  function hive_home {
    grep hive /etc/passwd | awk -F: '{print $6}'
  }
  h_home=$(hive_home)
  if [ ! -z $h_home ]; then
     export HIVE_HOME=$h_home
  fi
  alias bee='sudo -u hive beeline --color=true -u jdbc:hive2://'
  alias hvfs='sudo -u hive hadoop fs'
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

if (( $+commands[emacs] )); then
  alias em='emacs -nw'
  alias sem="$SUDO emacs -nw"
  export EDITOR='emacs -nw'
  if [ "$TERM" = 'dumb' ] && [ "$INSIDE_EMACS" ]; then
    export TERM='ansi-term'
  fi
elif (( $+commands[vim] )); then
  export EDITOR='vim'
fi

if (( $+commands[mc] )); then
  alias smc="$SUDO mc"
fi

if (( $+commands[git] )); then
  alias pll="git pull origin"
  alias psh="git push origin"
  alias gst="git status"
  alias gco="git checkout"
  alias gadd="git add ."
  alias gcmt="git commit -m"
fi

if [ -f ~/.zsh_aliases ]; then
  . ~/.zsh_aliases
fi

if [ -f /etc/profile.d/vte.sh ]; then
  . /etc/profile.d/vte.sh
fi

# PROMPT
# determine git branch name
function parse_git_branch(){
  git branch 2> /dev/null | sed -n 's/^\* //p'
}

# Determine the branch/state information for this git repository.
function set_git_branch() {
  # Get the name of the branch.
  branch="$(parse_git_branch)"
  if [ -n "${branch}" ]; then
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
    if [ "${modified_unstaged}" != 0 ]; then
      unstaged_files="%%${modified_unstaged}${unstaged_files}"
    fi
    if [ "${deleted_unstaged}" != 0 ]; then
      unstaged_files="-${deleted_unstaged}${unstaged_files}"
    fi
    if [ "${untracked_unstaged}" != 0 ]; then
      unstaged_files="*${untracked_unstaged}${unstaged_files}"
    fi
    # staged_files
    if [ "${modified_staged}" != 0 ]; then
      staged_files="%%${modified_staged}${staged_files}"
    fi
    if [ "${deleted_staged}" != 0 ]; then
      staged_files="-${deleted_staged}${staged_files}"
    fi
    if [ "${renamed_staged}" != 0 ]; then
      staged_files="^${renamed_staged}${staged_files}"
    fi
    if [ "${new_staged}" != 0 ]; then
      staged_files="+${new_staged}${staged_files}"
    fi
    if [ ! -z "${staged_files}" ]; then
      staged_files="|%F{green}${staged_files}%f"
    fi
    if [ ! -z "${unstaged_files}" ]; then
      unstaged_files="|%F{yellow}${unstaged_files}%f"
    fi
    if [ ! -z "${ahead}" ]; then
      ahead="%F{green}{>${ahead}}%f"
    fi
    if [ ! -z "${behind}" ]; then
      behind="%F{red}{<${behind}}%f"
    fi
    # Set the final branch string.
    echo " (%F{cyan}${branch}${ahead}${behind}${unstaged_files}${staged_files}%f)"
  fi

}
function set_prompt_symbol () {
  echo " %(?.%F{yellow}.%F{red}[%?])\n➤%f "
}
# Determine active Python virtualenv details.
function set_virtualenv () {
  if test -z "$VIRTUAL_ENV" ; then
    echo ""
  else
    echo " %F{yellow}[`basename \"$VIRTUAL_ENV\"`]"
  fi
}

# Set the full bash prompt.
function set_zsh_prompt () {
  PROMPT=' %F{yellow}%B%T%b%f$(set_virtualenv) %(!.%F{red}.%F{green})%n%f@%F{white}%m%f %F{magenta}{$(shrink_path -f)}%f$(set_git_branch)$(set_prompt_symbol)'
}
# Tell bash to execute this function just before displaying its prompt.
set_zsh_prompt
