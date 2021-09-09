if test -t 1; then
  # see if it supports colors...
  force_color_prompt=yes
  color_prompt=yes
  # enable color support of ls and also add handy aliases
  alias ls='ls --color=auto'
  alias less='less -R'
  alias dir='dir --color=auto'
  alias vdir='vdir --color=auto'
  alias grep='grep --color=auto'
  alias fgrep='fgrep --color=auto'
  alias egrep='egrep --color=auto'
fi

# ALIASES
alias less="less --LONG-PROMPT --no-init --quit-at-eof --quit-if-one-screen --quit-on-intr"

alias arch='uname -m'
alias ll='ls -ahlF --time-style=long-iso --group-directories-first'
alias la='ls -A'
alias ..='cd ..'
alias home_pr='cd $PERS_DIR/home'
alias compress_jpeg="find ./ -iname '*.jpg' -or -iname '*.jpeg' -type f -size +100k -exec jpeg-recompress --quality high --method ssim --accurate --min 70 {} {} \;"
alias compress_png="find ./ -iname '*.png' -type f -size +100k -exec optipng {} \;"
alias -g G='|grep'
alias -g L='|less'
alias check_adb='adb devices -l'
alias frx='firefox'
alias qte='qutebrowser'
alias vb='vieb'

if [ -f $ZDOTDIR/aliases.local.zsh ]; then
  . $ZDOTDIR/aliases.local.zsh
fi
