export ZDOTDIR=$HOME/.config/zsh

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
if [ -d $LOCAL_BIN ] && [[ $PATH != *"$LOCAL_BIN"* ]]; then
  export PATH=$PATH:$LOCAL_BIN
fi

CARGO_BIN=$HOME/.cargo/bin
if [ ! -d $CARGO_BIN ]; then
  mkdir -p $CARGO_BIN
fi
if [ -d $CARGO_BIN ] && [[ $PATH != *"$CARGO_BIN"* ]]; then
  export PATH=$PATH:$CARGO_BIN
fi

export GOPATH=$HOME/go
if [ -d $GOPATH/bin ] && [[ $PATH != *"$GOPATH/bin"* ]]; then
  export PATH=$PATH:$GOPATH/bin
fi
