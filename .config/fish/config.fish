set -xg MY_PROJECTS_ROOT /opt/work/projects

# common functions

# Docker
function run
    docker-compose run --service-ports $argv
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
end

function install_vim_bundles
    set -l ROOT $HOME/.vim/bundle
    set -l ROOT_NVIM $HOME/.config/nvim/bundle
    cd $ROOT/YouCompleteMe
    python2 install.py --gocode-completer
    cd $ROOT_NVIM/YouCompleteMe
    python2 install.py --gocode-completer
    cd $HOME
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

function projects
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
set -xg PATH $PATH $GOPATH $GOPATH/bin
set -xg TERM "xterm-256color"
set -x WORKON_HOME $HOME/.virtualenvs
set -x INFINALITY_FT_BRIGHTNESS "-10"
set -x INFINALITY_FT_FILTER_PARAMS "16 20 28 20 16"
eval (python2 -m virtualfish auto_activation global_requirements)
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
set fish_greeting ""
