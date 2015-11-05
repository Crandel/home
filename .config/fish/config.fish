set -xg MY_PROJECTS_ROOT /opt/work/projects

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
    xdg-open ~/Desktop/Sword\ of\ the\ Stars\ -\ Complete\ Collection.desktop
end

set -x EDITOR vim
set -x DE gnome
set -x BROWSER chromium
set -xg XDG_CONFIG_HOME $HOME/.config
set -xg XDG_DATA_HOME $HOME/.local
set -xg GOPATH $HOME/go
set -xg PATH $PATH $GOPATH
set -xg TERM "xterm-256color"
set -x WORKON_HOME $HOME/.virtualenvs
set -x INFINALITY_FT_BRIGHTNESS "-10"
set -x INFINALITY_FT_FILTER_PARAMS "16 20 28 20 16"
eval (python2 -m virtualfish auto_activation global_requirements)
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
set fish_greeting ""
