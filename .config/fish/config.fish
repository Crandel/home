set -x MY_PROJECTS_ROOT ~/work/projects

function projects
   cd $MY_PROJECTS_ROOT
end

function strl
   cd $MY_PROJECTS_ROOT/stroylandiya
end

function pyalg
   cd $MY_PROJECTS_ROOT/PythonAlgorithms
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

function auth
    cd $MY_PROJECTS_ROOT/auth
end

function rita
    cd $MY_PROJECTS_ROOT/rita
end

function rita_up
    cd $MY_PROJECTS_ROOT/rita
    paster serve --reload local.ini
end

function monup
    sudo chown -R mongodb: /opt/db/mongo/
    d start mongo
end

function servup
    monup
    cd $MY_PROJECTS_ROOT/rita
    sleep 3
    paster serve --reload local.ini
end

function geocp
    cp /home/crandel/work/projects/rita/mail/tmp/GeoLite2-City.mmdb /home/crandel/work/projects/rita/rita/public/GeoLite2-City.mmdb
end

function backup
    cd /media/ubuntu/home/vitaliy/work/backup
end

set -x EDITOR vim

set -xg GOPATH $HOME/go
set -xg PATH $PATH $GOPATH
set -x WORKON_HOME ~/.virtualenvs
eval (python2 -m virtualfish auto_activation global_requirements)
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
set fish_greeting ""
