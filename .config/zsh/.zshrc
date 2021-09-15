# ZSH SPECIFIC
# zmodload zsh/zprof
# start=`date +%s.%N`
# THE FOLLOWING LINES WERE ADDED BY COMPINSTALL
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*' menu select=2
zstyle ':completion:*' menu select=interactive
zstyle :compinstall filename '$ZDOTDIR/.zshrc'

# END OF LINES ADDED BY COMPINSTALL
setopt AUTOCD EXTENDEDGLOB NOTIFY PROMPT_SUBST INTERACTIVE_COMMENTS
setopt AUTO_NAME_DIRS CORRECTALL MAGIC_EQUAL_SUBST

bindkey -e

unsetopt nomatch # escape string fixing zsh: no matches found error

autoload -U colors && colors

autoload -Uz compinit
compinit

SPROMPT='Correct %B%F{red}%U%R%b%f%u to %F{green}%r%f? [%By%bes|%BN%bo|%Be%bdit|%Ba%bbort] '
# END ZSH SPECIFIC

# HISTORY
setopt HIST_IGNORE_ALL_DUPS SHARE_HISTORY INC_APPEND_HISTORY EXTENDED_HISTORY
setopt HIST_IGNORE_SPACE HIST_FIND_NO_DUPS HIST_SAVE_NO_DUPS
HISTFILE=$ZDOTDIR/.hist_zsh
HISTSIZE=5000000
SAVEHIST=$HISTSIZE
# END HISTORY

# LOCAL FUNCTIONS
# Arch Linux command-not-found support, you must have package pkgfile installed
[[ -e /usr/share/doc/pkgfile/command-not-found.zsh ]] && source /usr/share/doc/pkgfile/command-not-found.zsh

autoload edit-command-line
zle -N edit-command-line
bindkey '^h' edit-command-line

# NAVIGATION
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# NAVIGATION END


# CUSTOM FUNCTIONS
command_exists () {
  (( $+commands[$1] ))
}

# SUDO
SUDO=''
if [[ $EUID -ne 0 ]] && command_exists sudo ; then
  SUDO='sudo'
fi
# END SUDO

# IMPORT ADDITIONAL FILES
export LOCAL_ZSH_COMP_DIR=$HOME/.local/share/zsh/site-functions
if [ ! -d $LOCAL_ZSH_COMP_DIR ]; then
  mkdir -p $LOCAL_ZSH_COMP_DIR
fi
## CUSTOM FUNCS
if [ -f $ZDOTDIR/func.zsh ]; then
  . $ZDOTDIR/func.zsh
fi

## CUSTOM ALIASES
if [ -f $ZDOTDIR/aliases.zsh ]; then
  . $ZDOTDIR/aliases.zsh
fi

## APPS
if [ -f $ZDOTDIR/apps.zsh ]; then
  . $ZDOTDIR/apps.zsh
fi
# END IMPORT


# PLUGIN MANAGMENT
plugin_init() {
  source $zinit_source
  autoload -Uz _zinit
  (( ${+_comps} )) && _comps[zinit]=_zinit

  zinit wait lucid light-mode for \
          OMZL::clipboard.zsh \
          djui/alias-tips \
          zsh-users/zsh-completions \
          zsh-users/zsh-history-substring-search \
        atinit"ZINIT[COMPINIT_OPTS]=-C; zicompinit; zicdreplay" \
          zdharma/fast-syntax-highlighting \
        has'docker' \
          OMZP::docker/_docker \
        has'docker-compose' \
          OMZP::docker-compose \
        has'git' \
          OMZP::git \
        has'kubectl' \
          OMZP::kubectl \
        has'minikube' \
          OMZP::minikube \
        has'pip' \
          OMZP::pip

  zinit lucid light-mode for \
        OMZP::shrink-path \
        jeffreytse/zsh-vi-mode \
        blockf \
          zsh-users/zsh-completions \
        atload"!_zsh_autosuggest_start" \
        atinit"ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8,bold,bg=bold,underline'" \
          zsh-users/zsh-autosuggestions as"completion" \
        has'poetry' \
        load'[[ $(ls) = *pyproject.toml* ]]' \
          darvid/zsh-poetry

  compinit
}

zinit_source="$ZDOTDIR/.zinit/bin/zinit.zsh"
if [ ! -f $zinit_source ]; then
  mkdir $ZDOTDIR/.zinit
  git clone https://github.com/zdharma/zinit.git $ZDOTDIR/.zinit/bin/
fi
plugin_init
unfunction plugin_init


# PROMPT
function parse_git_branch(){
  git branch 2> /dev/null | sed -n 's/^\* //p'
}

## Determine the branch/state information for this git repository.
function set_git_branch() {
  # Get the final branch string.
  if command_exists git_status ; then
    branch="$(git_status zsh)"
  else
    branch="$(parse_git_branch)"
  fi

  if [ -n "${branch}" ]; then
    echo " ($branch)"
  fi
}

function set_prompt_symbol () {
  echo "%(?.%F{yellow}.%F{red}[%?])$INSIDE_VIFM$VI_MODE"
  echo "╰─➤%f"
}

fish_pwd() {
  if typeset -f shrink_path > /dev/null; then
    echo "$(shrink_path -f)"
  else
    echo "%~"
  fi
}

# Set the prompt.
set_zsh_prompt () {
  [[ $SSH_CONNECTION ]] && local uath='%F{white}@%M%f'
  PROMPT='%F{yellow}╭─%B%T%b%f$(set_virtualenv) %(!.%F{red}.%F{green})%n%f${uath} %F{magenta}{$(fish_pwd)}%f$(set_git_branch) $(set_prompt_symbol) '
}

# end=`date +%s.%N`
# printf "%.2f" $((end-start))
# Tell zsh to execute this function just before displaying its prompt.
set_zsh_prompt
