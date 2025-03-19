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
  alias grepf='grep -F --color=auto'
  alias grepe='grep -E --color=auto'
fi

# ALIASES
alias less="less --LONG-PROMPT --no-init --quit-at-eof --quit-if-one-screen --quit-on-intr"
alias which='type -p'

alias arch='uname -m'
alias la='ls -A'
alias ll='la -hlF --time-style=long-iso --group-directories-first'
alias ..='cd ..'
alias home_pr='cd $PERS_DIR/home'
alias compress_jpeg="fd -e jpg -e jpeg --size +100k --exec jpeg-recompress --quality high --method ssim --accurate --min 70 {} {} \;"
alias compress_png="fd -e png --size +100k --exec optipng {} \;"
alias -g G='|grep'
alias -g L='|less'
alias check_adb='adb devices -l'
alias bpon='bluetoothctl power on'
alias frx='firefox'
alias qte='qutebrowser'
alias vb='vieb'
alias cz='chezmoi'
alias cza='chezmoi apply'
alias czaf='chezmoi apply --force'
alias czd='chezmoi diff'
alias czm='chezmoi merge'
alias czs='chezmoi status'
alias scz='sudo chezmoi -D / -S $HOME/.local/share/chezmoi/root -c /root/.config/chezmoi/config.toml'
alias scza='scz apply'
alias sczaf='scz apply --force'
alias sczd='scz diff'
alias sczm='scz merge'
alias sczs='scz status'
alias crlt='curl -w "@$HOME/.config/curl-time-format"'
alias y='cd /data/media/youtube'

if command_exists pacman ; then
alias p="$SUDO pacman"
alias pql='pacman -Ql'
alias pqs='pacman -Qs'
alias pqi='pacman -Qii'
alias pss='pacman -Ss'
alias psu='p -Syu'
alias pii='pacman -Sii'
alias psi='p -S --needed'
alias prs='p -Rs'
if command_exists yay ; then
alias yay='yay --aur --editmenu --builddir $PERS_DIR/bb'
alias psuy='yay -Syua'
alias pssy='yay -Ss'
alias psiy='yay -Sa'
alias piiy='yay -Sii'
fi
if command_exists paru ; then
alias paru='paru --aur --fm vifm --removemake --clonedir $PERS_DIR/bb'
alias psuy='paru -Syua'
alias pssy='paru -Ss'
alias psiy='paru -Sa'
alias piiy='paru -Sii'
fi
fi
if command_exists apt ; then
  alias a="$SUDO apt"
  alias pql='dpkg-query -L'
  alias pqs='apt list --installed'
  alias pss='apt search'
  alias psd='a update'
  alias psy='a upgrade'
  alias psl='apt list --upgradable'
  alias psu='psd && sleep 2 && psl && sleep 2 && psy'
  alias psi='a install'
  alias prs='a remove'
  alias aar="$SUDO add-apt-repository"
fi

if command_exists yum ; then
  alias y="$SUDO yum"
  alias psu='yum upgrade'
  alias pss='yum search'
  alias psi='yum install'
  alias prs='yum remove'
fi
# END PACKAGE MANAGERS

# SYSTEM TOOLS
## VMs
### VAGRANT
### DOCKER
if command_exists docker ; then
alias d='docker'
compdef d='docker'
alias dst='d stop $(d ps -q)'
alias drm='d rm $(d ps -aq)'
alias dvrm='d volume rm $(d volume ls -q)'

alias dc='docker compose'
alias dcb="dc build"
alias dcdn="dc --all-resources down"
alias dce="dc exec"
alias dck="dc kill"
alias dcl="dc logs"
alias dclf="dc logs -f"
alias dcps="dc ps"
alias dcpull="dc pull"
alias dcr="dc run"
alias dcrestart="dc restart"
alias dcrm="dc rm"
alias dcrun="dc stop && dc run --service-ports"
alias dcstart="dc start"
alias dcstop="dc stop"
alias dcup="dc up"
alias dcupb="dc up --build"
alias dcupd="dc up -d"
fi
### KUBERNETES
if command_exists kubectl ; then
alias k='kubectl'
compdef k=kubectl
alias kapr='kubectl api-resources'
alias ktx='kubectx'
compdef ktx=kubectx
alias kns='kubens'
compdef kns=kubens
alias kss='k9s'
compdef kss=k9s
fi
## END VM's

## MEDIA TOOLS
alias ytdl='yt-dlp --embed-chapters --embed-subs'
alias ytlf='yt-dlp --list-formats'
alias ytls='yt-dlp --list-subs'
alias ytm='yt-dlp  -f bestaudio -x -P /data/media/youtube/music'
alias ytmp='yt-dlp -f bestaudio -x -P /data/media/youtube/music -o "%(playlist_index,autonumber)d.%(title).50s.%(ext)s"'
# --external-downloader aria2c --external-downloader-args "-x 10 -s 10"'
alias yt='ytdl  -f "b[height=720]/bv[height=720]+ba"'
alias yto='ytdl -f "b[height>=720]/bv[height>=720]+ba"'
alias ytl='ytdl -f "b[height>=1080]/bv[height>=1080]+ba" -o "%(upload_date>%d.%m)s.%(title)s.%(id)s.%(ext)s"'
alias ytp='ytdl -f "b[height=720]/bv[height=720]+ba" -P /data/media/youtube/playlists -o "%(playlist_index,autonumber)d.%(title).50s.%(id)s.%(ext)s"'
alias a2c='aria2c -x 10 -s 10'
alias i3s_rst='pkill -SIGUSR2 i3status-rs'
alias play='ffplay -nodisp -autoexit'
## END MEDIA TOOLS

## EDITORS
alias v='vim'
alias sv="$SUDO vim"
alias e='emacs -nw'
alias em='emacs -nw'
alias sem="$SUDO emacs -nw"
## END EDITORS

## FILE MANAGERS
alias smc="$SUDO mc"

alias vf="vifm"
compdef vf=vifm
alias svf="$SUDO vifm"
## END FILE MANAGERS
alias ssctl="$SUDO systemctl"
alias usctl='systemctl --user'
compdef usctl=systemctl
alias g='git'
compdef g=git
alias ga='g add'
alias gb='git branch'
alias gba='git branch -a'
alias gbd='git branch -d'
alias gbdf='git branch -D'
alias gco='git checkout'
alias gcb='git checkout -b'
alias gcm='git checkout "$(git_main_branch)"'
alias gcf='git config --list'
alias gcmt='git commit -v -a -m'
alias gcn='git clone'
alias gd='git diff'
alias gdca='git diff --cached'
alias gf='git fetch'
alias gfo='git fetch origin'
alias gfc='git fetch origin "$(git_current_branch)"'
alias gfm='git fetch origin "$(git_main_branch)"'
alias gl='git pull'
alias glo='git pull origin'
alias glc='git pull origin "$(git_current_branch)"'
alias glm='git pull origin "$(git_main_branch)"'
alias glr='git pull --rebase'
alias gm='git merge'
alias gp='git push'
alias gpo='git push origin'
alias gpc='git push origin "$(git_current_branch)"'
alias gpm='git push origin "$(git_main_branch)"'
alias gr='git remote'
alias gra='git remote add'
alias grv='git remote -v'
alias grb='git rebase'
alias grba='git rebase --abort'
alias grbc='git rebase --continue'
alias grbi='git rebase -i'
alias grhh='git reset --hard'
alias grpo='git remote prune origin'
alias gs='git status'
alias gsb='git status -sb'
alias tm='tmux attach || tmux new'
alias gen_mirror='reflector --ipv4 --country Germany --age 12 -p https -l 10 --sort score --save /tmp/mirrorlist'
alias gen_rsync='reflector --ipv4 --country Germany --age 12 -p rsync -l 10 --sort score --save /tmp/powerpill'
alias olstt="$SUDO systemctl start ollama.service"
alias olend="$SUDO systemctl stop ollama.service"
alias olsts="$SUDO systemctl status ollama.service"
alias olc='ollama cp'
alias olf='journalctl -f -u ollama'
alias oll='ollama list'
alias olp='ollama pull'
alias olps='ollama ps'
alias olr='ollama rm'
# END SYSTEM TOOLS

# PROGRAMM LANGUAGES
## RUST
alias crn='cargo run'
alias cup='cargo update'
alias cbd='cargo build'
alias cbr='cargo build --release'
alias ct='bat'
compdef ct=bat
alias fsf='sk'
eval "$(zoxide init --no-aliases zsh)"
alias j='__zoxide_z' # cd to highest ranked directory matching path
alias ja='__zoxide_za' # add path to the database
alias ji='__zoxide_zi' # cd with interactive selection using fzf
alias jr='zoxide remove' # remove path from the database
alias l='lsd'
compdef l=lsd
alias ll='l -ahlF --group-dirs=first'
## END RUST

## GO
alias task='go-task'
compdef task=go-task
## END GO
## PYTHON
if [ -d "$HOME/.pyenv" ]; then
  eval "$(pyenv init --path)"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
fi

if [ -f $ZDOTDIR/aliases.local.zsh ]; then
  . $ZDOTDIR/aliases.local.zsh
fi
