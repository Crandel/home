function projects 
   cd /home/crandel/work/projects
end

function upg
   sudo pacman -Syu
end

function upgy
   yaourt -Syua
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

function catalog
   cd /home/crandel/work/projects/catalog
end

function esper
   cd /home/crandel/work/projects/espermasters
end

set -x EDITOR vim
set -xg GOPATH $HOME/go
set -xg PATH $PATH $GOPATH
set -x WORKON_HOME ~/.virtualenvs
eval (python2 -m virtualfish auto_activation global_requirements)
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on'
set fish_greeting ""
