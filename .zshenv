# ZSH Specific envs
export ZDOTDIR=$HOME/.config/zsh

declare -A ZINIT
export ZINIT[HOME_DIR]=$ZDOTDIR/.zinit

export ZPFX=$ZINIT
typeset -U path # tells the shell that it should not add anything to $path if it's there already.
SPROMPT='Correct %B%F{red}%U%R%b%f%u to %F{green}%r%f? [%By%bes|%BN%bo|%Be%bdit|%Ba%bbort] '
export WORDCHARS='*?[]~&;!#$%^(){}<>'
export HISTFILE=$ZDOTDIR/.hist_zsh
export HISTSIZE=5000000
export SAVEHIST=$HISTSIZE
export LOCAL_ZSH_COMP_DIR=$HOME/.local/share/zsh/site-functions
export KEYTIMEOUT=1
# Zsh envs end

if test -t 1; then
  export TERM="xterm-256color"
  export CLICOLOR=1
  export LS_COLORS=$LS_COLORS:"di=01;35"
  if (( $+commands[tput] ))
  then
    # Start blinking
    export LESS_TERMCAP_mb=$(tput bold; tput setaf 2) # green
    # Start bold
    export LESS_TERMCAP_md=$(tput bold; tput setaf 2) # green
    # Start stand out
    export LESS_TERMCAP_so=$(tput bold; tput setaf 3) # yellow
    # End standout
    export LESS_TERMCAP_se=$(tput rmso; tput sgr0)
    # Start underline
    export LESS_TERMCAP_us=$(tput smul; tput bold; tput setaf 1) # red
    # End Underline
    export LESS_TERMCAP_ue=$(tput sgr0)
    # End bold, blinking, standout, underline
    export LESS_TERMCAP_me=$(tput sgr0)
  fi
fi
export PAGER='less -SRXF'
export EDITOR='vim'

export PERS_DIR='/data/work'

LOCAL_BIN=$HOME/.local/bin
if [ ! -d $LOCAL_BIN ]; then
  mkdir -p $LOCAL_BIN
fi
export PATH=$PATH:$LOCAL_BIN

CARGO_BIN=$HOME/.cargo/bin
if [ ! -d $CARGO_BIN ]; then
  mkdir -p $CARGO_BIN
fi
export PATH=$PATH:$CARGO_BIN

export GOPATH=$HOME/go
export PATH=$PATH:$GOPATH/bin
