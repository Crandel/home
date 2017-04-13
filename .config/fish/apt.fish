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

function paci
	apt install $argv
end
