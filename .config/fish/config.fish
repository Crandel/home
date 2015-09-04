set -x MY_PROJECTS_ROOT ~/work/projects

function projects 
   cd $MY_PROJECTS_ROOT
end

function strl
   cd $MY_PROJECTS_ROOT/stroylandiya
end

function bung
   cd $MY_PROJECTS_ROOT/bungalow
end

function pass
   cd $MY_PROJECTS_ROOT/password
end

function catalog
   cd $MY_PROJECTS_ROOT/catalog
end

function esper
   cd $MY_PROJECTS_ROOT/espermasters
end

function rita
    cd $MY_PROJECTS_ROOT/rita
end

function rita_up
    cd $MY_PROJECTS_ROOT/rita
    paster serve --reload local.ini
end

function monup
    sudo systemctl start mongodb.service
    sudo systemctl status -l mongodb.service
end

function serup
    paster serve --reload local.ini
end

set -x EDITOR vim

set -xg GOPATH $HOME/go
set -xg PATH $PATH $GOPATH
set -x WORKON_HOME ~/.virtualenvs
eval (python2 -m virtualfish auto_activation global_requirements)
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
set fish_greeting ""
