## Path variables
set -xg MY_PROJECTS_ROOT /opt/work/projects

set -q XDG_DATA_HOME; or set -l XDG_DATA_HOME $HOME/.local/share
set -q XDG_CONFIG_HOME; or set -l XDG_CONFIG_HOME $HOME/.config

set -q fish_data_path; or set -g fish_data_path $XDG_DATA_HOME/fish
set -q fish_config_path; or set -g fish_config_path $XDG_CONFIG_HOME/fish

set -l GOPATH1 $HOME/go
set -xg MY_GO_PROJECTS_ROOT $GOPATH1/goprojects
set -xg GOPATH $GOPATH1:$MY_GO_PROJECTS_ROOT

set -x WORKON_HOME $HOME/.virtualenvs

set -x JAVA_HOME /usr/lib/jvm/default
set -x HADOOP_USER_NAME hadoop
set -x HIVE_HOME /usr/lib/hive
set -x SCALA_HOME /usr/share/scala

set -xg PATH $PATH $SCALA_HOME/bin $GOPATH1/bin

## Functional variables
set -xg GO_VENDOR 1
set -x EDITOR 'emacs -nw'
set -x BROWSER chromium
#set -xg RUST $HOME/rust
set -xg TERM "xterm-256color"
set -xg WINEARCH "win32"

set -x GNOME_DESKTOP_SESSION_ID 1
eval (python2 -m virtualfish auto_activation global_requirements)
set fish_greeting ""

## cdhist options
set -g fish_cdhist_path $fish_data_path/fish_cdhist
set -g fish_cdhist_max 128

# common functions

# different checkers
function sudo_run
  if test (id -u) -eq 0
    eval $argv
  else
    sudo $argv
  end
end

if type -pq pacman
	source $fish_config_path/pacman.fish
end

# Docker
function d
	docker $argv
end

function dc
	if count $argv > /dev/null
		docker-compose $argv
	else
		docker-compose config
	end
end

function run
	if count $argv > /dev/null
		set -l path $MY_PROJECTS_ROOT/$argv
		if test -d $path
			cd $path
		end
	end
	dc stop
	dc run --rm --service-ports app
end

function dl
	if count $argv > /dev/null
		dc logs --tail 15 $argv
	else
		dc logs --tail 15
	end
end

# End Docker

# Vagrant
function vup
	vagrant up
end

function vh
	vagrant halt
end

function vs
	vagrant ssh
end
# End Vagrant
function extract
	if set -q argv
		switch $argv
			case '*.tar.bz2'
				tar -xvjf $argv
			case '*.tar.gz'
				tar -xvzf $argv
			case '*.tar.xz'
				tar -xvJf $argv
			case '*.bz2'
				bunzip2 $argv
			case '*.rar'
				unrar x $argv
			case '*.gz'
				gunzip $argv
			case '*.tar'
				tar xvf $argv
			case '*.tbz2'
				tar xvjf $argv
			case '*.tgz'
				tar xvzf $argv
			case '*.zip'
				unzip $argv
			case '*.Z'
				uncompress $argv
			case '*.7z'
				7z x $argv
			case '*.xz'
				unxz $argv
			case '*.exe'
				cabextract $argv
			case '*'
				echo $argv ": unrecognized file compression"
		end
	end
end

function zipin
	for f in *
		switch $f
			case '*zip'
				echo $f
			case '*'
				zip -9 $f.zip $f
				rm $f
		end
	end
end

function backup
	if count $argv > /dev/null
		cd /opt/work/backup/$argv
	else
		cd /opt/work/backup/
	end
end

function go_path
	cd ~/go
end

function home_pr
	cd /opt/work/home
end

function group_fix
	sudo_run grpck
	sudo_run pwck
end

## Aliases
function ls --description 'List contents of directory'
	set -l param --color=auto
	if isatty 1
		set param $param --indicator-style=classify
	end
	if [ (uname -o) = "GNU/Linux" ]
		set param $param --group-directories-first
	end
	command ls $param $argv
end

function l
	ls -CF $argv
end

function la
	ls -A $argv
end

function ll
	ls -alF $argv
end

function hm -d "Merge history from several shells"
	history --merge
end

function pr -d "project directory"
	# if argv when go to directory
	set -l path $MY_PROJECTS_ROOT
	if count $argv > /dev/null
		set -x path $path"/"$argv
		#switch (echo $argv)
		#case cashback
		#		 set -l check (docker inspect -f "{{.State.Running}}" postgres)
		#		 if [ $check = "false" ]
		#				 docker start postgres
		#		 end
		#case rita
		#		 set -l check (docker inspect -f "{{.State.Running}}" mongo)
		#		 if [ $check = "false" ]
		#				 docker start mongo
		#		 end
		#case photoculture
		#		 set -l check (docker inspect -f "{{.State.Running}}" photo_db)
		#		 if [ $check = "false" ]
		#				 docker start photo_db
		#		 end
		#end
	end
	if test -d $path
		cd $path
	end
end

function spr
	# if argv when go to directory
	set -l path $MY_PROJECTS_ROOT/scala
	if count $argv > /dev/null
		set -x path $path"/"$argv
	end
	if test -d $path
		cd $path
	end
end

function gpr
	# if argv when go to directory
	set -l path $MY_GO_PROJECTS_ROOT
	if count $argv > /dev/null
		set -x path $path"/src/"$argv
		switch (echo $argv)
			case go_chat
				set -l check (docker inspect -f "{{.State.Running}}" postgres)
				if [ $check = "false" ]
					docker start postgres
				end
		end
	end
	if test -d $path
		cd $path
	end
end

function rmv
	sudo_run mv $argv /tmp
end

function soff
	sudo_run swapoff /dev/sda4
end

function son
	sudo_run swapon /dev/sda4
end

function systemctl
	sudo_run systemctl $argv
end

function tm
	tmux attach
	tmux new
end

function em
	emacs -nw $argv
end

function sem
	sudo_run emacs -nw $argv
end

function smc
	sudo_run -E mc
end

function update_kernel
	sudo_run mkinitcpio -p linux
end

# # rita
# function geocl
#			git checkout rita/public/GeoLite2-City.mmdb
# end

# function geocp
#			cp $MY_PROJECTS_ROOT/rita/mail/tmp/GeoLite2-City.mmdb $MY_PROJECTS_ROOT/rita/rita/public/GeoLite2-City.mmdb
# end

# function monup
#			sudo_run chown -R mongodb: /opt/db/mongo/
#			set -l check (docker inspect -f "{{.State.Running}}" mongo)
#			if [ $check = "false" ]
#					echo $check
#					docker start mongo
#			end
# end

# function servup
#			cd $MY_PROJECTS_ROOT/rita
#			set -lx MONGODB_ADDON_URI "mongodb://mongo/rita"
#			set -lx VIRTUAL_ENV ""
#			set -lx PORT "8060"
#			monup
#			python run.py serve
# end

# function rita_temp
#			cd /opt/work/env/rita/lib/python2.7/site-packages
#			rm -rf marrow.templating-1.0.2-py2.7-nspkg.pth marrow.templating-1.0.2-py2.7.egg-info/ marrow/templating/
#			cp /opt/work/backup/rita/marrow.templating-1.0.2-py2.7.egg /opt/work/env/rita/lib/python2.7/site-packages/
#			cd /opt/work/projects/rita
# end
# rita end
# localhost
function hdmi_on
	xrandr --output eDP1 --off --output HDMI1 --mode 1920x1080 --primary --dpi 96 --set "Broadcast RGB" "Full"
end

function hdmi_off
	xrandr --output eDP1 --auto --primary --output HDMI1 --off
end

function both_on
	xrandr --output eDP1 --dpi 150 --mode 1920x1080 --primary --output HDMI1 --mode 1920x1080 --dpi 96 --right-of eDP1 --set "Broadcast RGB" "Full"
end

function openvpn_run
	sudo_run openvpn --config /etc/openvpn/client/client.conf
end

function internet
	cd /media/data/internet
end

function torrent
	cd /media/data/torrent
end

function work
	cd /media/data/work
end

function civ
	cd /media/data/games/Civilisation5/
	optirun ./Civ5XP
end

function pret
	cd /media/data/games/Praetorians
	optirun wine Praetorians.exe
end

function sword
	cd /media/data/games/SwordoftheStars
	optirun env WINEPREFIX="/home/crandel/.wine" wine /media/data/games/SwordoftheStars/Sword\ of\ the\ Stars.exe
end
function sword2
	cd /media/data/games/SwordOfTheStars\ 2.EnhancedEdition
	optirun wine bin/x86/sots2.exe
end

# localhost end
# start X at login
if status --is-login
	if test -z "$DISPLAY" -a $XDG_VTNR -eq 1
		exec /bin/bash startx -- -keeptty
	end
end

# if test -z "$DESKTOP_SESSION"
#	eval (gnome-keyring-daemon --start)
#	set -x SSH_AUTH_SOCK
# end


function fish_title
	echo $_ ' '
	echo (prompt_pwd)
end
