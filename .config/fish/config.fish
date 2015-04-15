function ll
   ls -alF $argv
end
 
function la
   ls -A $argv
end
 
function l
   ls -CF $argv
end
 
function virt
   cd /media/data/virtual
end

function upg
   sudo pacman -Syu
end

function upgy
   yaourt -Syua
end

function up
   docker-compose up
end

function internet
   cd /media/data/internet
end

function torrent
   cd /media/data/torrent
end

function bung
   cd /media/data/virtual/work/bungalow
end

function pass
   cd /media/data/virtual/work/pass
end

function go_path
   cd ~/go
end

function catalog
   cd /media/data/virtual/work/catalog
end

function esper
   cd /media/data/virtual/work/espermasters
end

function tm

   tmux attach
   tmux new
end
set -x EDITOR vim 
set -xg GOPATH $HOME/go 
set -xg PATH $PATH $GOPATH
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=setting'
set fish_greeting ""
