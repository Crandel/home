set -xg MY_PROJECTS_ROOT /opt/work/projects

function projects
   cd $MY_PROJECTS_ROOT
end

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
    grpck
    pwck
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

function hm
    history --merge
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
    mv $argv /tmp
end

function soff
    swapoff /dev/sda5
end

function son
   swapon /dev/sda5
end

function tm
   tmux attach
   tmux new
end

function em
   emacs -nw $argv
end

function update_kernel
    mkinitcpio -p linux
end

function upg
    pacman -Syu
end

set -x EDITOR 'emacs -nw'
set -xg GOPATH /home/crandel/go
set -xg PATH $PATH $GOPATH
set -xg TERM "xterm-256color"
# fix emacs dumb term
if test "$TERM" = "dumb"
    set -xg TERM "ansi-term"
end
set fish_greeting ""

function fish_title
    echo $_ ' '
    echo (prompt_pwd)
end
