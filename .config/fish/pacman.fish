set -l listinstalled "(pacman -Q | string replace ' ' \t)"
set -l listall "(__fish_print_packages)"

function pacman
	sudo_run pacman $argv
end

function pacfiles -d 'List of files in pacman package packages sorted by size'
	pacman -Qlq $argv | grep -v '/$' | xargs du -cbh | sort -h
end
complete -c pacfiles -a "$listinstalled"

function pel
	set -l result
	pacman -Qlq $argv | grep -v '/$' | eval (__fzfcmd) -m --tiebreak=index --toggle-sort=ctrl-r | while read -l r; set result $result $r; end
	[ "$result" ]; and eval $EDITOR $result
end
complete -c pel -a "$listinstalled"

function pli -a size -d 'Pacman last installed packages'
	[ $size ]; or set size 30
	expac -t '%F %T' '%-8l %n' | sort -rn | head -$size
end

function pqi
	pacman -Qi $argv
end
complete -c pqi -a "$listinstalled"

function pql -d 'Get detailed file list (no folders) for the specified packages'
	pacman -Ql $argv | grep -v '/$'
end
complete -c pql -a "$listinstalled"

function pqo
	pacman -Qo $argv
end
complete -c pqo -a '(__fish_complete_command)'

function pqs
	pacman -Qs $argv
end

function pacr
	pacman -Rs $argv
end
complete -c pacr -a "$listinstalled"

function psi
	pacman -Si $argv
end
complete -c psi -a "$listall"

function paci
	pacman -S --needed $argv
end
complete -c paci -a "$listall"

function yaci
	yaourt -Sa $argv
end
complete -c yaci -a "$listall"

function pacs
	pacman -Ss $argv
end

function yacs --description 'Search using yaourt'
	yaourt -Ss $argv
end

function upg -d 'Run pacman system update'
	pacman -Syu
end

function upgy --description 'Run yaourt system update'
	yaourt -Syua
end
