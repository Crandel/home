# ZSH SPECIFIC
# zmodload zsh/zprof
# start=`date +%s.%N`


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
ZSH_CUSTOM_PLUGINS=$XDG_CACHE_HOME/zsh/plugins
ZSH_CUSTOM_COMPLETIONS=$XDG_CACHE_HOME/zsh/completions
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
compinit

# IMPORT ADDITIONAL FILES
## CUSTOM FUNCS
source $ZDOTDIR/func.zsh

## CUSTOM ALIASES
source $ZDOTDIR/aliases.zsh
# END IMPORT

bindkey -e
bindkey "^h"      edit-command-line
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

bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history

# PROMPT
eval "$(starship init zsh)"

# end=`date +%s.%N`
# printf "%.2f" $((end-start))
# Tell zsh to execute this function just before displaying its prompt.
