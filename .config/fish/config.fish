set -xg MY_PROJECTS_ROOT /opt/work/projects

# common functions

# Docker
function run
    docker-compose run --rm --service-ports $argv
end

function dl
    docker-compose logs
end

function ds
    docker-compose stop
end

function up
    docker-compose up -d
end

function upl
    docker-compose logs &
    docker-compose up
end
# End Docker

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

function backup
   cd /opt/work/backup/$argv
end

function go_path
   cd ~/go
end

function home_pr
    cd /opt/work/home
end

function group_fix
    sudo grpck
	sudo pwck
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

function pacman
    sudo pacman $argv
end

function pr
    # if argv when go to directory
    if count $argv > /dev/null
        cd $MY_PROJECTS_ROOT/$argv
    else
        cd $MY_PROJECTS_ROOT
    end
end

function rmv
    sudo mv $argv /tmp
end

function soff
    sudo swapoff /dev/sda5
end

function son
   sudo swapon /dev/sda5
end

function systemctl
    sudo systemctl $argv
end

function tm
   tmux attach
   tmux new
end

function update_kernel
    sudo mkinitcpio -p linux
end

function upg
    sudo pacman -Syu
end

function upgy
    yaourt -Syua
end


# rita
function geocl
    git checkout rita/public/GeoLite2-City.mmdb
end

function geocp
    cp $MY_PROJECTS_ROOT/rita/mail/tmp/GeoLite2-City.mmdb $MY_PROJECTS_ROOT/rita/rita/public/GeoLite2-City.mmdb
end

function monup
    sudo chown -R mongodb: /opt/db/mongo/
    d start mongo
end

function servup
    cd $MY_PROJECTS_ROOT/rita
    paster serve --reload local.ini
end

function rita_temp
    cd /opt/work/env/rita/lib/python2.7/site-packages
    rm -rf marrow.templating-1.0.2-py2.7-nspkg.pth marrow.templating-1.0.2-py2.7.egg-info/ marrow/templating/
    cp /opt/work/backup/rita/marrow.templating-1.0.2-py2.7.egg .
    cd /opt/work/projects/rita
end

function rita
    nvim -S ~/.vim/sessions/rita.vim
end
# rita end
# localhost
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
    primusrun ./Civ5XP
end

function sword
    cd /media/data/games/SwordoftheStars
    primusrun env WINEPREFIX="/home/crandel/.wine" wine /media/data/games/SwordoftheStars/Sword\ of\ the\ Stars.exe
end
# localhost end

set -x EDITOR vim
set -x DE gnome
set -x BROWSER chromium
set -xg XDG_CONFIG_HOME $HOME/.config
set -xg XDG_DATA_HOME $HOME/.local
set -xg GOPATH $HOME/go
set -xg RUST $HOME/rust
set -xg PATH $PATH $GOPATH $GOPATH/bin $RUST/bin
set -xg TERM "xterm-256color"
set -x WORKON_HOME $HOME/.virtualenvs
set -x INFINALITY_FT_BRIGHTNESS "-10"
set -x INFINALITY_FT_FILTER_PARAMS "16 20 28 20 16"
eval (python2 -m virtualfish auto_activation global_requirements)
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
set fish_greeting ""
