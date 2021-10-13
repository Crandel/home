# ZSH SPECIFIC
# zmodload zsh/zprof
# start=`date +%s.%N`
# THE FOLLOWING LINES WERE ADDED BY COMPINSTALL
zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:descriptions' format '%U%K{yellow} %F{green}-- %F{red} %BNICE!1! %b%f %d --%f%k%u'
zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS}

zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
# zstyle ':completion:*' menu select=interactive
zstyle :compinstall filename '$ZDOTDIR/.zshrc'

# END OF LINES ADDED BY COMPINSTALL
setopt AUTO_CD # Go to folder path without using cd.
setopt AUTO_NAME_DIRS # Any parameter that is set to the absolute name of a directory immediately becomes a name for that directory
setopt AUTO_PUSHD # Push the old directory onto the stack on cd.
setopt CDABLE_VARS # Change directory to a path stored in a variable.
setopt CORRECT # Spelling correction
setopt CORRECTALL
setopt EXTENDED_GLOB  # Use extended globbing syntax.
setopt GLOB_DOTS # Do not require a leading ‘.’ in a filename to be matched explicitly.
setopt INTERACTIVE_COMMENTS # Allow comments even in interactive shells
setopt MAGIC_EQUAL_SUBST # All unquoted arguments of the form ‘anything=expression’ appearing after the command name have filename expansion
setopt NOTIFY # Report the status of background jobs immediately, rather than waiting until just before printing a prompt.
setopt PROMPT_SUBST # If set, parameter expansion, command substitution and arithmetic expansion are performed in prompts.
setopt PUSHD_IGNORE_DUPS # Do not store duplicates in the stack.
setopt PUSHD_SILENT # Do not print the directory stack after pushd or popd.


unsetopt nomatch # escape string fixing zsh: no matches found error

autoload -U colors
colors

zmodload zsh/complist

autoload -Uz compinit
compinit

# END ZSH SPECIFIC

# HISTORY
setopt EXTENDED_HISTORY          # Write the history file in the ':start:elapsed;command' format.
setopt HIST_EXPIRE_DUPS_FIRST    # Expire a duplicate event first when trimming history.
setopt HIST_FIND_NO_DUPS         # Do not display a previously found event.
setopt HIST_IGNORE_ALL_DUPS      # Delete an old recorded event if a new event is a duplicate.
setopt HIST_IGNORE_DUPS          # Do not record an event that was just recorded again.
setopt HIST_IGNORE_SPACE         # Do not record an event starting with a space.
setopt HIST_SAVE_NO_DUPS         # Do not write a duplicate event to the history file.
setopt SHARE_HISTORY             # Share history between all sessions.
# END HISTORY

bindkey -e

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
        blockf \
          zsh-users/zsh-completions \
        atload"!_zsh_autosuggest_start" \
        atinit"ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE='fg=8,bold,underline'" \
          zsh-users/zsh-autosuggestions as"completion" \
        has'docker' as"completion" \
          OMZP::docker/_docker \
        as"completion" \
          OMZP::docker-compose/_docker-compose \
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
bindkey -e
# bindkey 'jk'      exit_insert_mode
bindkey "^h"      edit-command-line
bindkey "^["      vi-cmd-mode
# \e[A arrow up
# \e[B arrow down
# \e[C arrow right
# \e[D arrow left
bindkey "\e[A"    up-line-or-beginning-search
bindkey "\e[B"    down-line-or-beginning-search
# \e[1;5A Ctrl + arrow up
# \e[1;5B Ctrl + arrow down
# \e[1;5C Ctrl + arrow right
# \e[1;5D Ctrl + arrow left
bindkey "\e[1;5A" history-substring-search-up
bindkey "\e[1;5B" history-substring-search-down
bindkey "\e[1;5C" forward-word
bindkey "\e[1;5D" backward-word
# \e[1;2A Shift + arrow up
# \e[1;2B Shift + arrow down
# \e[1;2C Shift + arrow right
# \e[1;2D Shift + arrow left
bindkey "\e[1;2A" history-incremental-search-forward
bindkey "\e[1;2B" history-incremental-search-backward # Ctrl+r
bindkey "\e[1;2C" end-of-line
bindkey "\e[1;2D" beginning-of-line

# fix of delete key
bindkey "\e[3~"   delete-char
# bindkey  "\e[3;5~" delete-word
bindkey "^[^?"    backward-kill-word
# bindkey "^@"      autosuggest-accept

function zle-keymap-select() {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'underline' ]]; then
    VI_MODE="{N}"
    echo -ne '\e[3 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    VI_MODE="{I}"
    echo -ne '\e[5 q'
  fi
  zle reset-prompt
}

function zle-line-init(){
    VI_MODE="{I}"
    echo -ne '\e[5 q'
    zle reset-prompt
}

zle -N zle-line-init
zle -N zle-keymap-select

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
# end=`date +%s.%N`
# printf "%.2f" $((end-start))
# Tell zsh to execute this function just before displaying its prompt.
set_zsh_prompt
