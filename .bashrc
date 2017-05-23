# ~/.bashrc: executed by bash(1) for non-login shells.

# COLORS
RED=""
YELLOW=""
GREEN=""
BLUE=""
PURPLE=""
LIGHT_RED=""
LIGHT_GREEN=""
WHITE=""
LIGHT_GRAY=""
NORMAL=""
# check if stdout is a terminal...
if test -t 1; then
	# see if it supports colors...
	ncolors=$(tput colors)
	if test -n "$ncolors" && test $ncolors -ge 8; then
		export TERM="xterm"
		force_color_prompt=yes
		color_prompt=yes
		NORMAL="\[\e[0m\]"
		BLACK="$(tput setaf 0)"
		RED="$(tput setaf 1)"
		LIGHT_RED="$(tput setaf 1)"
		GREEN="$(tput setaf 2)"
		LIGHT_GREEN="$(tput setaf 2)"
		YELLOW="$(tput setaf 3)"
		BLUE="$(tput setaf 4)"
		MAGENTA="$(tput setaf 5)"
		PURPLE="$(tput setaf 5)"
		CYAN="$(tput setaf 6)"
		WHITE="$(tput setaf 7)"
		LIGHT_GRAY="$(tput setaf 7)"
		# enable color support of ls and also add handy aliases
		bind 'set colored-completion-prefix on'
		bind 'set colored-stats on'
		alias ls='ls --color=auto'
		alias dir='dir --color=auto'
		alias vdir='vdir --color=auto'

		alias grep='grep --color=auto'
		alias fgrep='fgrep --color=auto'
		alias egrep='egrep --color=auto'
	fi
	if test -n "$ncolors" && test $ncolors -gt 8; then
		export TERM="xterm-256color"
		BLUE="$(tput setaf 12)"
		PURPLE="$(tput setaf 53)"
		LIGHT_RED="$(tput setaf 9)"
		LIGHT_GREEN="$(tput setaf 10)"
		LIGHT_GRAY="$(tput setaf 8)"
	fi
fi

# HISTORY
# don't put duplicate lines or lines starting with space in the history.
# for setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTCONTROL=ignoreboth
HISTSIZE=
HISTFILESIZE=
HISTTIMEFORMAT="%F %T "

# append to the history file, don't overwrite it
shopt -s histappend
shopt -s checkwinsize
shopt -s cdspell
shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

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


# ALIASES
# some more ls aliases
alias arch='uname -m'
alias ll='ls -ahlF'
alias la='ls -A'
alias ~='cd $HOME'

if [ -f ~/.bash_aliases ]; then
	. ~/.bash_aliases
fi

if [ -f ~/clusterdock.sh ]; then
	. ~/clusterdock.sh
fi

# FUNCTIONS
function command_exists () {
	type "$1" &> /dev/null ;
}

function pr () {
	local projects_folder="/opt/work/projects/"
	cd $projects_folder
	if [ ! -z $1 ]; then
		cd $1
	fi
}

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


# BINS CONDITIONS
SUDO=''
if [[ $EUID -ne 0 ]] && command_exists sudo ; then
	complete -cf sudo
	SUDO='sudo'
fi

if command_exists pacman ; then
	alias pacman="$SUDO pacman"
	alias upg='pacman -Syu'
	alias upgy='yaourt -Syu'
	alias pacs='pacman -Ss'
	alias paci='pacman -S --needed'
fi

if command_exists apt ; then
	alias apt="$SUDO apt"
	alias upgy='apt update'
	alias upg='upgy && apt upgrade'
	alias pacs='apt search'
	alias paci='apt install'
	alias pql="dpkg-query -L"
fi

if command_exists yum ; then
	alias apt="$SUDO yum"
	alias upg='yum upgrade'
	alias pacs='yum search'
	alias paci='yum install'
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
	alias dst='d stop (d ps -q)'
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


virtual='/usr/bin/virtualenvwrapper.sh'
if [ -f $virtual ]; then
	export VIRTUAL_ENV_DISABLE_PROMPT=1
	export WORKON_HOME=~/.virtualenvs/
	. $virtual
fi

if command_exists go ; then
	export GOPATH=$HOME/go
	export PATH=$PATH:$GOPATH/bin
fi

if command_exists hadoop ; then
	alias hdp='sudo -u hdfs hadoop fs'
fi

if command_exists hive ; then
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

if command_exists spark-shell ; then
	alias spll='sudo -u spark spark-shell'
	alias spmit='sudo -u spark spark-submit'
fi

if [ -d /usr/share/scala ]; then
	export SCALA_HOME=/usr/share/scala
	export PATH=$PATH:$SCALA_HOME/bin
fi

if [ -d /usr/lib/jvm/default ]; then
	export JAVA_HOME=/usr/lib/jvm/default
elif [ -d /usr/lib/jvm/default-java ]; then
	export JAVA_HOME=/usr/lib/jvm/default-java
fi

if command_exists emacs; then
	alias em='emacs -nw'
	alias sem="$SUDO em"
	export EDITOR='emacs -nw'
	if [ "$TERM" = 'dumb' ] && [ "$INSIDE_EMACS" ]; then
		export TERM='ansi-term'
	fi
elif command_exists vim; then
	export EDITOR='vim'
fi

if command_exists mc; then
	alias smc="$SUDO mc"
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

# PROMPT
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
		BRANCH=" (${CYAN}${branch}${NORMAL}${ahead}${behind}${unstaged_files}${staged_files}) "
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
