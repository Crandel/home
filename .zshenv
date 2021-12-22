# ZSH Specific envs
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export ZDOTDIR=$XDG_CONFIG_HOME/zsh
export ZSH_CACHE_DIR="$XDG_CACHE_HOME/zsh"

typeset -U path # tells the shell that it should not add anything to $path if it's there already.
SPROMPT='Correct %B%F{red}%U%R%b%f%u to %F{green}%r%f? [%By%bes|%BN%bo|%Be%bdit|%Ba%bbort] '
export WORDCHARS='*?[]~&;!#$%^(){}<>'
export HISTFILE=$ZDOTDIR/.hist_zsh
export HISTSIZE=5000000
export SAVEHIST=$HISTSIZE
export LOCAL_ZSH_COMP_DIR="$XDG_CACHE_DIR/zsh/site-functions"
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
export EDITOR='vim'
export EDITOR='emacs'
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_PLATFORM_PLUGIN="qt5ct"
export BEMENU_OPTS='-I 0 -i --fn "Hack:32" --nb "#1e1e1e" --nf "#c0f440" --sf "#1e1e1e" --sb "#f4800d" --tb "#d7dd90" --tf "#111206" --hb "#49088c" --hf "#c2fbd3"'
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc

export GO111MODULE=on
export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git'"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export PYENV_ROOT="$HOME/.pyenv"
export PATH="$PYENV_ROOT/bin:$PATH"
export PYENV_VIRTUALENV_DISABLE_PROMPT=1
export NPM_PACKAGES="${XDG_DATA_HOME}"
export NODE_PATH="$NPM_PACKAGES/lib/node_modules"
