# ZSH Specific envs
skip_global_compinit=1
export XDG_CONFIG_HOME="$HOME/.config"
export XDG_DATA_HOME="$HOME/.local/share"
export XDG_CACHE_HOME="$HOME/.cache"

export ZDOTDIR=$XDG_CONFIG_HOME/zsh
export ZSH_CACHE_DIR="$XDG_CACHE_HOME/zsh"
if [ ! -d $ZSH_CACHE_DIR ]; then
  mkdir -p $ZSH_CACHE_DIR
fi

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
export EDITOR='vim'
export EDITOR='emacs'
export LSP_USE_PLISTS=true
export QT_QPA_PLATFORMTHEME="qt6ct"
export QT_PLATFORM_PLUGIN="qt6ct"
export XCURSOR_THEME=capitaine-cursors
export XCURSOR_SIZE=30
export MANROFFOPT="-c"
export MANPAGER="sh -c 'col -bx | bat -l man -p'"
export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
export GOPATH="$HOME/.local/share/go"
export GOBIN=$LOCAL_BIN
export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git'"
export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
export WINEARCH=win32
if [ -f . $ZDOTDIR/zshenv.local.zsh ]; then
  . $ZDOTDIR/zshenv.local.zsh
fi

