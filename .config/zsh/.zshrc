# DEBUG
# zmodload zsh/zprof
# start=`date +%s.%N`

zstyle ':completion:*' completer _expand _complete _ignored _approximate
zstyle ':completion:*' matcher-list '' '' 'm:{[:lower:]}={[:upper:]} m:{[:lower:][:upper:]}={[:upper:][:lower:]} r:|[._-]=** r:|=** l:|=*'
zstyle ':completion:*:*:*:*:descriptions' format '%F{green}-- %d --%f'
zstyle ':completion:*:*:*:*:corrections' format '%F{yellow}!- %d (errors: %e) -!%f'
zstyle ':completion:*:descriptions' format '%U%K{yellow} %F{green}-- %F{red} %BNICE!1! %b%f %d --%f%k%u'
zstyle ':completion:*:default' list-colors "${(s.:.)LS_COLORS}"

zstyle ':completion:*' group-name ''
zstyle ':completion:*' menu select=2
zstyle ':completion:*' rehash true
zstyle :compinstall filename '$ZDOTDIR/.zshrc'

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

typeset -U path # tells the shell that it should not add anything to $path if it's there already.

autoload -Uz colors
colors

zmodload zsh/complist

autoload -Uz compinit
autoload -Uz edit-command-line
zle -N edit-command-line

# NAVIGATION
autoload -Uz up-line-or-beginning-search down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# NAVIGATION END

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

# Quoted urls
set zle_bracketed_paste

autoload -Uz bracketed-paste-magic url-quote-magic

zle -N bracketed-paste bracketed-paste-magic
zle -N self-insert url-quote-magic

# CUSTOM FUNCTIONS
command_exists () {
  (( $+commands[$1] ))
}

# SUDO
export SUDO=''
if [[ $EUID -ne 0 ]] && command_exists sudo ; then
  export SUDO='sudo'
fi
# END SUDO

# PLUGIN MANAGMENT
ZSH_CUSTOM_PLUGINS=$ZSH_CACHE_DIR/plugins
ZSH_CUSTOM_COMPLETIONS=$ZSH_CACHE_DIR/completions
ZSH_CUSTOM_PROMPT=$ZSH_CACHE_DIR/prompt
alias src='source'
if [ -d $ZSH_CUSTOM_PLUGINS/zsh-defer ]; then
  alias src='zsh-defer source'
  source $ZSH_CUSTOM_PLUGINS/zsh-defer/zsh-defer.plugin.zsh
fi
if [ -d $ZSH_CUSTOM_PLUGINS/alias-tips ]; then
  src $ZSH_CUSTOM_PLUGINS/alias-tips/alias-tips.plugin.zsh
fi
if [ -d $ZSH_CUSTOM_PLUGINS/zsh-autosuggestions ]; then
  src $ZSH_CUSTOM_PLUGINS/zsh-autosuggestions/zsh-autosuggestions.plugin.zsh
fi
if [ -d $ZSH_CUSTOM_PLUGINS/zsh-history-substring-search ]; then
  src $ZSH_CUSTOM_PLUGINS/zsh-history-substring-search/zsh-history-substring-search.plugin.zsh
fi
if [ -d $ZSH_CUSTOM_PLUGINS/fast-syntax-highlighting ]; then
  src $ZSH_CUSTOM_PLUGINS/fast-syntax-highlighting/fast-syntax-highlighting.plugin.zsh
fi

if [ -d $ZSH_CUSTOM_PLUGINS/ohmyzsh ]; then
  ohmyplugins="$ZSH_CUSTOM_PLUGINS/ohmyzsh/plugins"
  src "$ohmyplugins/command-not-found/command-not-found.plugin.zsh"
  if command_exists docker; then
    fpath=( "$ohmyplugins/docker" $fpath )
    fpath=( "$ohmyplugins/docker-compose" $fpath )
  fi
  if command_exists go; then
    src "$ohmyplugins/golang/golang.plugin.zsh"
  fi
  if command_exists kubectl; then
    src "$ohmyplugins/kubectl/kubectl.plugin.zsh"
  fi
fi

if [ -d $ZSH_CUSTOM_COMPLETIONS/zsh-completions ]; then
  fpath=( "$ZSH_CUSTOM_COMPLETIONS/zsh-completions/src" $fpath )
fi

compinit -u

# IMPORT ADDITIONAL FILES
## CUSTOM FUNCS
source $ZDOTDIR/func.zsh

## CUSTOM ALIASES
source $ZDOTDIR/aliases.zsh
# END IMPORT

bindkey -e
bindkey "^h" edit-command-line
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

# bindkey "^["      vi-cmd-mode
# function zle-keymap-select() {
#   if [[ ${KEYMAP} == vicmd ]] ||
#      [[ $1 = 'underline' ]]; then
#     echo -ne '\e[3 q'
#   elif [[ ${KEYMAP} == main ]] ||
#        [[ ${KEYMAP} == viins ]] ||
#        [[ ${KEYMAP} = '' ]] ||
#        [[ $1 = 'beam' ]]; then
#     echo -ne '\e[5 q'
#   fi
#   zle reset-prompt
# }

# function zle-line-init(){
#     echo -ne '\e[5 q'
#     zle reset-prompt
# }

# zle -N zle-line-init
# zle -N zle-keymap-select

# bindkey -M menuselect 'h' vi-backward-char
# bindkey -M menuselect 'k' vi-up-line-or-history
# bindkey -M menuselect 'l' vi-forward-char
# bindkey -M menuselect 'j' vi-down-line-or-history

# PROMPT
if [ -d $ZSH_CUSTOM_PROMPT/powerlevel10k ]; then
  POWERLEVEL9K_DISABLE_CONFIGURATION_WIZARD=true
  source $ZSH_CUSTOM_PROMPT/powerlevel10k/powerlevel10k.zsh-theme
  source $ZDOTDIR/.p10k.zsh
else
  echo "$ZSH_CUSTOM_PROMPT/powerlevel10k directory is missing"
fi
# eval "$(starship init zsh)"

# end=`date +%s.%N`
# printf "%.2f" $((end-start))
# Tell zsh to execute this function just before displaying its prompt.
