function man --description 'Color and justify  the on-line manual pages'
	## Work around the "builtin" manpage that everything symlinks to,
	## by prepending our fish datadir to man. This also ensures that man gives fish's
	## man pages priority, without having to put fish's bin directories first in $PATH

	# Justify man pager to current window size.
	set -q MANWIDTH; and [ $COLUMNS -lt $MANWIDTH ]; and set -lx MANWIDTH $COLUMNS

	## Color when using LESS.
	## mb = ?
	## md = bold (titles, commands)
	## so = status bar
	## us = italic (arguments, files)
	set -lx LESS_TERMCAP_mb (printf "\e[1;31m")
	set -lx LESS_TERMCAP_md (printf "\e[0;36m")
	set -lx LESS_TERMCAP_me (printf "\e[0m")
	set -lx LESS_TERMCAP_se (printf "\e[0m")
	set -lx LESS_TERMCAP_so (printf "\e[1;4;37m")
	set -lx LESS_TERMCAP_ue (printf "\e[0m")
	set -lx LESS_TERMCAP_us (printf "\e[0;33m")

	# Notice local but exported variable
	set -lx MANPATH (string join : $MANPATH)
	if test -z "$MANPATH"
		type -q manpath
		and set MANPATH (command manpath)
	end
	set -l fish_manpath (dirname $__fish_datadir)/fish/man
	if test -d "$fish_manpath" -a -n "$MANPATH"
		set MANPATH "$fish_manpath":$MANPATH
	end

	# If fish's man pages could not be found, just invoke man normally
	command man $argv
end
