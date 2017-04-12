# The following lines were added by compinstall
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle :compinstall filename '/home/crandel/.zshrc'

autoload -Uz compinit
compinit
# End of lines added by compinstall
# Lines configured by zsh-newuser-install
HISTCONTROL=ignoreboth
HISTFILE=~/.hist_zsh
HISTSIZE=10000
SAVEHIST=10000
setopt autocd extendedglob notify PROMPT_SUBST
bindkey -e
autoload -Uz promptinit
promptinit
# End of lines configured by zsh-newuser-install
# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"
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

command_exists () {
	type "$1" &> /dev/null ;
}

if command_exists pacman ; then
	alias pacman='sudo pacman'
	alias upg='pacman -Syu'
	alias upgy='yaourt -Syu'
fi

if command_exists apt ; then
	alias apt='sudo apt'
	alias upgy='apt update'
	alias upg='upgy && apt upgrade'
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
fi

if command_exists vagrant ; then
	# Vagrant
	alias vup='vagrant up'
	alias vsus='vagrant suspend'
	alias vre='vagrant reload'
	alias vsh='vagrant ssh'
fi

if command_exists systemctl ; then
	alias systemctl='sudo systemctl'
fi

alias backup='cd /opt/work/backup'
alias pr='cd /opt/work/projects; cd'

if command_exists virtualenvwrapper ; then
	export VIRTUAL_ENV_DISABLE_PROMPT=1
	export WORKON_HOME=~/.virtualenvs/
	virtual='/usr/bin/virtualenvwrapper.sh'
	if [ -f $virtual ]; then
		. $virtual
	fi
fi

if command_exists go ; then
	export GOPATH=$HOME/go
	export PATH=$PATH:$GOPATH/bin
fi

if [ -d /usr/lib/jvm/default ]; then
	export JAVA_HOME=/usr/lib/jvm/default
elif [ -d /usr/lib/jvm/default-java ]; then
	export JAVA_HOME=/usr/lib/jvm/default-java
fi

if command_exists emacs; then
	alias em='emacs -nw'
	export EDITOR=em
elif command_exists vim; then
	export EDITOR='vim'
fi

export TERM="xterm-256color"
if [ "$TERM" = 'dumb' ] && [ "$INSIDE_EMACS" ]; then
	export TERM='ansi-term'
fi
# Alias definitions.
# You may want to put all your additions into a separate file like
# ~/.bash_aliases, instead of adding them here directly.
# See /usr/share/doc/bash-doc/examples in the bash-doc package.

if [ -f ~/.zsh_aliases ]; then
	. ~/.zsh_aliases
fi


if [ -f ~/clusterdock.sh ]; then
	. ~/clusterdock.sh
fi

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
			staged_files="%${modified_staged}${staged_files}"
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
			ahead="%F{cyan}{>${ahead}}%f"
		fi
		if [ ! -z "${behind}" ]; then
			behind="%F{red}{<${behind}}%f"
		fi
		# Set the final branch string.
		echo "%F{cyan}(${branch}${ahead}${behind}${unstaged_files}${staged_files})%f "
	fi

}
function set_prompt_symbol () {
	echo "%(?.%F{green}.%F{red}[%?])
➤%f "
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
	PROMPT=' %F{blue}%B%T%b%f$(set_virtualenv) %(!.%F{red}.%F{green})%n%f %F{magenta}{%~}%f%F{cyan}$(set_git_branch)%f %F{white}Z%f $(set_prompt_symbol)'
}
# Tell bash to execute this function just before displaying its prompt.
set_zsh_prompt
