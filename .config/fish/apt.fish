set -l listinstalled "(apt-mark showmanual)"
set -l listall "(__fish_print_packages)"

function apt
	sudo_run apt $argv
end

function upgy
	apt update
end

function upg
	upgy
	apt upgrade
end

function pacs
	apt search $argv
end
complete -c pacs -a "$listall"

function pacr
	apt remove $argv
end
complete -c pacr -a "$listinstalled"

function paci
	apt install $argv
end
complete -c paci -a "$listall"