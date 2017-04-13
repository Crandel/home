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

function pss
	apt search $argv
end

function psi
	apt install $argv
end
