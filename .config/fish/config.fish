set -xg MY_PROJECTS_ROOT /home/crandel/work/projects

function projects
   cd $MY_PROJECTS_ROOT
end

function geocp
    cp $MY_PROJECTS_ROOT/rita/mail/tmp/GeoLite2-City.mmdb $MY_PROJECTS_ROOT/rita/rita/public/GeoLite2-City.mmdb
end

function geocl
    git checkout rita/public/GeoLite2-City.mmdb
end

set -x EDITOR vim

set -xg GOPATH $HOME/go
set -xg PATH $PATH $GOPATH
set -xg TERM "xterm-256color"
set -x WORKON_HOME $HOME/.virtualenvs
eval (python2 -m virtualfish auto_activation global_requirements)
set -xg _JAVA_OPTIONS '-Dawt.useSystemAAFontSettings=on -Dswing.aatext=true -Dswing.defaultlaf=com.sun.java.swing.plaf.gtk.GTKLookAndFeel'
set fish_greeting ""
