# ~/.bashrc: executed by bash(1) for non-login shells.
        RED="\[\033[0;31m\]"
     YELLOW="\[\033[1;33m\]"
      GREEN="\[\033[0;32m\]"
       BLUE="\[\033[1;34m\]"
     PURPLE="\[\033[0;35m\]"
  LIGHT_RED="\[\033[1;31m\]"
LIGHT_GREEN="\[\033[1;32m\]"
      WHITE="\[\033[1;37m\]"
 LIGHT_GRAY="\[\033[0;37m\]"
 COLOR_NONE="\[\e[0m\]"

# don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# append to the history file, don't overwrite it
shopt -s histappend
complete -cf sudo
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=10000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# set a fancy prompt (non-color, unless we know we "want" color)
# uncomment for a colored prompt, if the terminal has the capability; turned
# off by default to not distract the user: the focus in a terminal window
# should be on the output of commands, not on the prompt
force_color_prompt=yes
color_prompt=yes

# enable color support of ls and also add handy aliases
alias ls='ls --color=auto'
alias dir='dir --color=auto'
alias vdir='vdir --color=auto'

alias grep='grep --color=auto'
alias fgrep='fgrep --color=auto'
alias egrep='egrep --color=auto'
alias arch='uname -m'

# some more ls aliases
alias ll='ls -ahlF'
alias la='ls -A'

alias upg='sudo pacman -Syu'
alias upgy='yaourt -Syu'
alias tm='tmux attach || tmux new'
# Docker
alias d='docker'
alias dc='docker-compose'
alias dl='docker-compose logs --tail 15'
alias run='docker-compose stop && docker-compose run --rm --service-ports app'
# Docker end
# Vagrant
alias vup='vagrant up'
alias vsus='vagrant suspend'
alias vre='vagrant reload'
alias vsh='vagrant ssh'
# Vagrant end
alias backup='cd /opt/work/backup'
alias systemctl='sudo systemctl'
alias pacman='sudo pacman'
alias pr='cd /opt/work/projects; cd'

export EDITOR='emacs -nw'
export WORKON_HOME=~/.virtualenvs/
export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
export JAVA_HOME=/usr/lib/jvm/default
export TERM="xterm-256color"
if [ "$TERM" = 'dumb' ] && [ "$INSIDE_EMACS" ]; then
    export TERM='ansi-term'
fi
virtual='/usr/bin/virtualenvwrapper.sh'
if [ -f $virtual ]; then
    . $virtual
fi
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

if [ -f ~/clusterdock.sh ]; then
    . ~/clusterdock.sh 
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

# get current status of git repo
function parse_git_dirty {
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
}

# determine git branch name
function parse_git_branch(){
  git branch 2> /dev/null | sed -n 's/^\* //p'
}

# Determine the branch/state information for this git repository.
function set_git_branch() {
  # Get the name of the branch.
  branch=$(parse_git_branch)
  BRANCH=''
  if [ ! "${branch}" == "" ]; then
      staged_files=''
      unstaged_files=''
      parse_git_dirty
      if [ ! "${staged_files}" == "" ]; then
          staged_files="|${GREEN}${staged_files}${COLOR_NONE}"
      fi
      if [ ! "${unstaged_files}" == "" ]; then
          unstaged_files="|${YELLOW}${unstaged_files}${COLOR_NONE}"
      fi
      if [ ! "${ahead}" == "" ]; then
          ahead="${LIGHT_GREEN}{>${ahead}}${COLOR_NONE}"
      fi
      if [ ! "${behind}" == "" ]; then
          behind="${LIGHT_RED}{<${behind}}${COLOR_NONE}"
      fi
      # Set the final branch string.
      BRANCH="(${BLUE}${branch}${COLOR_NONE}${ahead}${behind}${unstaged_files}${staged_files}) "
  fi

}

# Return the prompt symbol to use, colorized based on the return value of the
# previous command.
function set_prompt_symbol () {
  if test $1 -eq 0 ; then
      PROMPT_SYMBOL="${GREEN}
➤${COLOR_NONE}"
  else
      PROMPT_SYMBOL="${LIGHT_RED}[$1]
➤${COLOR_NONE}"
  fi
}


# Determine active Python virtualenv details.
function set_virtualenv () {
  if test -z "$VIRTUAL_ENV" ; then
      PYTHON_VIRTUALENV=""
  else
      PYTHON_VIRTUALENV=" ${YELLOW}[`basename \"$VIRTUAL_ENV\"`]${COLOR_NONE}"
  fi
}

# Set the full bash prompt.
function set_bash_prompt () {
  # Set the PROMPT_SYMBOL variable. We do this first so we don't lose the
  # return value of the last command.
  set_prompt_symbol $?

  # Set the PYTHON_VIRTUALENV variable.
  set_virtualenv

  # Set the BRANCH variable.
  set_git_branch

  # Set the bash prompt variable.
  PS1=" ${BLUE}\A${COLOR_NONE}${PYTHON_VIRTUALENV} ${GREEN}\u${COLOR_NONE} ${PURPLE}{\w}${COLOR_NONE}${BRANCH} B ${PROMPT_SYMBOL} "
}

# Tell bash to execute this function just before displaying its prompt.
PROMPT_COMMAND=set_bash_prompt
