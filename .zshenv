export ZDOTDIR=$HOME/.config/zsh

typeset -U path # tells the shell that it should not add anything to $path if it's there already.

export WORDCHARS='*?[]~&;!#$%^(){}<>'

if test -t 1; then
  export TERM="xterm-256color"
  export CLICOLOR=1
  export LS_COLORS=$LS_COLORS:"di=01;35"
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
