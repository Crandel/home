function ll
   ls -alF $argv
end
 
function la
   ls -A $argv
end
 
function l
   ls -CF $argv
end
 
function projects 
   cd /home/crandel/work/projects
end

function upg
   sudo pacman -Syu
end

function upgy
   yaourt -Syua
end

function son
   sudo swapon /dev/sda5
end

function soff
    sudo swapoff /dev/sda5
end

function update_plugin
    cd ~/.vim/bundle
    pwd
    for i in (command ls);
        cd $i;
        git pull
        git submodule update --init --recursive
        cd ..
    end
end

function up
   docker-compose up
end

function strl
   cd /home/crandel/work/projects/stroylandiya
end

function bung
   cd /home/crandel/work/projects/bungalow
end

function pass
   cd /home/crandel/work/projects/password
end

function go_path
   cd ~/go
end

function catalog
   cd /home/crandel/work/projects/catalog
end

function esper
   cd /home/crandel/work/projects/espermasters
end

function tm
   tmux attach
   tmux new
end
set -x EDITOR vim 
set -xg GOPATH $HOME/go 
set -xg PATH $PATH $GOPATH
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on'
set fish_greeting ""
