# PACKAGE MANAGERS
if command_exists pacman ; then
  alias p="$SUDO pacman"
  alias pql='pacman -Ql'
  alias pqs='pacman -Qs'
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
    alias paru='paru --aur --fm vim --clonedir $PERS_DIR/bb'
    alias psuy='paru -Syua'
    alias pssy='paru -Ss'
    alias psiy='paru -Sa'
    alias piiy='paru -Sii'
  fi


  paclist() {
    pacman -Qq | fzf --preview 'pacman -Qil {}' --layout=reverse --bind 'enter:execute(pacman -Qil {} | less)'
  }

  recovery-pacman() {
    pacman "$@"  \
           --log /dev/null   \
           --noscriptlet     \
           --dbonly          \
           --force           \
           --nodeps          \
           --needed
  }
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
if command_exists vagrant ; then
  alias vup='vagrant up'
  alias vh='vagrant halt'
  alias vsus='vagrant suspend'
  alias vre='vagrant reload'
  alias vs='vagrant ssh'
fi

### DOCKER
if command_exists docker ; then
  alias d='docker'
  compdef d='docker'
  alias dc='docker compose'
  alias drun='dc stop && dc run --service-ports'
  alias dst='d stop $(d ps -q)'
  alias drm='d rm $(d ps -aq)'
  alias dvrm='d volume rm $(d volume ls -q)'
  drmin () {
    for img in $(d images | rg -i 'none' | awk '{print $3}'); do
      docker rmi $img
    done
  }
  d_exec() {
    docker exec -it $1 sh -c "stty cols $COLUMNS rows $LINES && sh -l";
  }
  export d_exec;
  d_ip() {
    doc_ip=$(ip a show docker0 | grep "inet " | awk '{split($2, a, "/"); print a[1]}')
    export DOCKER_HOST_IP=$doc_ip
  }
  export d_ip
fi

### KUBERNETES
if command_exists kubectl ; then
  alias kapr='kubectl api-resources'
  if command_exists kubectx ; then
    alias ktx='kubectx'
    compdef ktx=kubectx
  fi
  if command_exists kubens ; then
    alias kns='kubens'
    compdef kns=kubens
  fi
  # alias k9s_cont_namesp='k9s --context <context> -n <namespace>'
  # pod_pf() {
  #   context=""
  #   namespace=""
  #   pod=$(kubectl get pod -l name=<pod name> \
  #                         --context $context                     \
  #                         -n $namespace                          \
  #                         -o jsonpath="{.items[0].metadata.name}")
  #   echo "pod is $pod"
  #   kubectl port-forward $pod --context $context -n $namespace 8000:8000
  # }
fi

if command_exists aws ; then
  alias aelogin='aws ecr get-login --region eu-central-1'
  if command_exists saml2aws ; then
    export SAML2AWS_SESSION_DURATION=36000
    alias sl='saml2aws login -a default -p default -r eu-central-1 --skip-prompt'
  fi
fi
## END VM's

## MEDIA TOOLS
if command_exists youtube-dl ; then
  alias ytdl='youtube-dl'
  alias ytb='ytdl -f "bestvideo[height<=1080]"+bestaudio' # --external-downloader aria2c --external-downloader-args "-x 10 -s 10"'
  alias ytm='ytdl -f bestaudio -x'
  alias ytlf='ytdl --list-formats'
fi

if command_exists yt-dlp ; then
  alias ypdl='yt-dlp'
  alias ypb='ypdl -f "bestvideo[height<=1080]"+bestaudio' # --external-downloader aria2c --external-downloader-args "-x 10 -s 10"'
  alias ypm='ypdl -f bestaudio -x'
  alias yplfp='ypdl --list-formats'
fi

if command_exists aria2c ; then
  alias a2c='aria2c -x 10 -s 10'
fi

if command_exists ffplay ; then
  alias play='ffplay -nodisp -autoexit'
fi
## END MEDIA TOOLS

## EDITORS
if command_exists vim; then
  alias v='vim'
  alias sv="$SUDO vim"
  export EDITOR='vim'
fi
if command_exists emacs ; then
  alias em='emacs -nw'
  alias e='emacs -nw'
  alias sem="$SUDO emacs -nw"
  export EDITOR='emacs'
fi
## END EDITORS

## FILE MANAGERS
if command_exists mc ; then
  alias smc="$SUDO mc"
fi

if command_exists vifm ; then
  alias vf="vifm"
  alias svf="$SUDO vifm"
  if [ -n "$INSIDE_VIFM" ]; then
    export INSIDE_VIFM="[VF]"
  fi
fi

if command_exists nnn ; then
  alias nnn='nnn -d'
  export NNN_USE_EDITOR=1
  export NNN_CONTEXT_COLORS='2745'
  export NNN_COPIER=$(which xsel)
  export NNN_NOTE=/opt/work/backup/notes
  export NNN_OPS_PROG=1
fi
## END FILE MANAGERS

if command_exists systemctl ; then
  alias ssctl="$SUDO systemctl"
  alias usctl='systemctl --user'
fi

if command_exists git ; then
  alias g='git'
  compdef g=git
  alias pla='g pull'
  alias pll='pla origin'
  alias psh='g push origin'
  alias gst='g status'
  alias gco='g checkout'
  alias gadd='g add'
  alias gcmt='g commit -m'
fi

if command_exists tmux ; then
  alias tm='tmux attach || tmux new'
fi

if command_exists qt5ct ; then
  export QT_QPA_PLATFORMTHEME="qt5ct"
  export QT_PLATFORM_PLUGIN="qt5ct"
fi

if command_exists clipmenud ; then
  export CM_LAUNCHER=bemenu
  export CM_DIR=$HOME/.cache/.clipmenud
fi

if command_exists bemenu ; then
  export BEMENU_OPTS='-I 0 -i --fn "Hack:26" --nb "#1e1e1e" --nf "#c0f440" --sf "#1e1e1e" --sb "#f4800d" --tb "#d7dd90" --tf "#111206" --hb "#49088c" --hf "#c2fbd3"'
fi

if command_exists reflector ; then
  alias gen_mirror='reflector --ipv4 --country Germany --age 12 -p https -l 10 --sort score --save /tmp/mirrorlist'
  alias gen_rsync='reflector --ipv4 --country Germany --age 12 -p rsync -l 10 --sort score --save /tmp/powerpill'
fi

if command_exists shiori ; then
  export SHIORI_DIR=$BACKUP_DIR/drive/sync
fi
# END SYSTEM TOOLS

# PROGRAMM LANGUAGES
## RUST

if command_exists cargo || [ -d $HOME/.rustup ]; then
  alias crn='cargo run'
  alias cup='cargo update'
  alias cbd='cargo build'
  alias cbr='cargo build --release'
  setup_cargo () {
    rustup completions zsh cargo  > $LOCAL_ZSH_COMP_DIR/_cargo
    rustup completions zsh rustup > $LOCAL_ZSH_COMP_DIR/_rustup
  }
  setup_cargo_tools() {
    if ! command_exists cargo-expand; then
      cargo install cargo-expand
    fi
    if ! command_exists cargo-audit; then
      cargo install cargo-audit
    fi
    if ! command_exists cargo-outdated; then
      cargo install cargo-outdated
    fi
  }
fi

if [ -d /usr/src/rust ]; then
  export RUST_SRC_PATH=/usr/src/rust/src
fi

if command_exists bat ; then
  alias ct='bat'
  export MANPAGER="sh -c 'col -bx | bat -l man -p'"
fi

if ! command_exists tldr ; then
  echo "install tealdeer"
fi

if ! command_exists rg ; then
  echo "install ripgrep"
else
  export RIPGREP_CONFIG_PATH=$HOME/.ripgreprc
fi

if command_exists sk ; then
  alias fzf='sk'
  if [[ -d /usr/share/skim/completion.zsh ]]; then
    source /usr/share/skim/completion.zsh
  fi
  gdelbrs() {
    git branch |
      rg --invert-match '\*' |
      cut -c 3- |
      sk --multi --preview="git log {} --" |
      xargs --no-run-if-empty git branch --delete --force
  }
else
  echo "install sk"
fi

if command_exists zoxide; then
  eval "$(zoxide init --no-aliases zsh)"
  alias j='__zoxide_z' # cd to highest ranked directory matching path
  alias ja='__zoxide_za' # add path to the database
  alias ji='__zoxide_zi' # cd with interactive selection using fzf
  alias jr='__zoxide_zr' # remove path from the database
else
  echo "Please install zoxide"
fi

if command_exists lsd; then
  alias l='lsd'
  alias ll='l -ahlF --group-dirs=first'
fi
## END RUST

## GO
go16_path=/usr/lib/go-1.16/bin
if [ -d $go16_path ]; then
  export PATH=$PATH:$go16_path
fi

if command_exists go ; then
  export GO111MODULE=on
fi

if command_exists fzf ; then
  export FZF_DEFAULT_COMMAND="rg --files --hidden --glob '!.git'"
  export FZF_CTRL_T_COMMAND="$FZF_DEFAULT_COMMAND"
  gdelbrf() {
    git branch |
      rg --invert-match '\*' |
      cut -c 3- |
      fzf --multi --preview="git log {} --" |
      xargs --no-run-if-empty git branch --delete --force
  }
else
  echo "install fzf"
fi

if command_exists chezmoi ; then
  alias cz='chezmoi'
fi

if command_exists lf ; then
  lfcd () {
    tmp="$(mktemp)"
    lf -last-dir-path="$tmp" "$@"
    if [ -f "$tmp" ]; then
      dir="$(cat "$tmp")"
      rm -f "$tmp"
      [ -d "$dir" ] && [ "$dir" != "$(pwd)" ] && cd "$dir"
    fi
  }
  bindkey -s '^o' 'lfcd\n'
fi
## END GO

## SCALA
if [ -d /usr/share/scala ]; then
  export SCALA_HOME=/usr/share/scala
  export PATH=$PATH:$SCALA_HOME/bin
fi

if command_exists scala ; then
  alias s='scala'
  alias sc='scalac'
  srun() {
    name=$@
    echo "Start compilation for $name"
    scalac "$name.scala"
    echo "Compilation done"
    scala $name
  }
  if command_exists coursier; then
    alias openapi-generator-cli="coursier launch org.openapitools:openapi-generator-cli:latest.release --"
  fi
fi
## END SCALA

## PYTHON
if [ -d "$HOME/.pyenv" ]; then
  export PYENV_ROOT="$HOME/.pyenv"
  export PATH="$PYENV_ROOT/bin:$PATH"
  export PYENV_VIRTUALENV_DISABLE_PROMPT=1
  eval "$(pyenv init --path)"
  eval "$(pyenv init - zsh)"
  eval "$(pyenv virtualenv-init - zsh)"
fi

clean_pyc (){
  find . | grep -E "(__pycache__|\.pyc|\.pyo$)" | xargs rm -rf
}

### Determine active Python virtualenv details.
set_virtualenv () {
  if [ ! -z "$PYENV_VIRTUAL_ENV" ] ; then
    echo " %F{yellow}[$(pyenv version-name)]%f"
  fi
}
## END PYTHON

## NPM
if command_exists npm; then
  NPM_PACKAGES="${HOME}/.local"
  export NODE_PATH="$NPM_PACKAGES/lib/node_modules"
fi
# END PROGRAMM LANGUAGES
