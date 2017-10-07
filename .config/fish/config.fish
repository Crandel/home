## Path variables
set -xg MY_PROJECTS_ROOT /opt/work/projects

set -q XDG_DATA_HOME; or set -l XDG_DATA_HOME $HOME/.local/share
set -q XDG_CONFIG_HOME; or set -l XDG_CONFIG_HOME $HOME/.config

set -q fish_data_path; or set -g fish_data_path $XDG_DATA_HOME/fish
set -q fish_config_path; or set -g fish_config_path $XDG_CONFIG_HOME/fish

if test -d /usr/lib/jvm/default
  set -x JAVA_HOME /usr/lib/jvm/default
else if test -d /usr/lib/jvm/default-java
  set -x JAVA_HOME /usr/lib/jvm/default-java
end

if type -pq hadoop
  set -x HADOOP_USER_NAME hadoop
end

if type -pq hive; and test -f /usr/lib/hive
  set -x HIVE_HOME /usr/lib/hive
end

if type -pq scala; and test -f /usr/share/scala
  set -x SCALA_HOME /usr/share/scala
  set -xg PATH $PATH $SCALA_HOME/bin
end

## Functional variables
if type -pq chromium
  set -x BROWSER chromium
else if type -pq chromium-browser
  set -x BROWSER chromium-browser
end

# Rust
if test -d $HOME/.cargo/bin
  set -xg PATH $PATH $HOME/.cargo/bin
end

if test -d /usr/src/rust
  set -x RUST_SRC_PATH /usr/src/rust/src
end
# End Rust

set -xg TERM "xterm-256color"
if type -pq wine
  set -xg WINEARCH "win32"
end

set -x GNOME_DESKTOP_SESSION_ID 1

if type -pq virtualenvwrapper.sh
  set -x WORKON_HOME $HOME/.virtualenvs
end

if [ (python2 -c 'import pkgutil; print(1 if pkgutil.find_loader("virtualfish") else 0)') = "1" ]
  eval (python2 -m virtualfish auto_activation global_requirements)
end

set fish_greeting ""

## cdhist options
set -g fish_cdhist_path $fish_data_path/fish_cdhist
set -g fish_cdhist_max 128

# common functions

# different checkers
function sudo_run
  if test (id -u) -eq 0
    eval $argv
  else
    sudo $argv
  end
end

if type -pq pacman; and test -f $fish_config_path/pacman.fish
  source $fish_config_path/pacman.fish
end

if type -pq apt; and test -f $fish_config_path/apt.fish
  source $fish_config_path/apt.fish
end

if type -pq docker; and test -f $fish_config_path/docker.fish
  source $fish_config_path/docker.fish
end

if type -pq vagrant; and test -f $fish_config_path/vagrant.fish
  source $fish_config_path/vagrant.fish
end

if type -pq go; and test -f $fish_config_path/go.fish
  source $fish_config_path/go.fish
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
    if test -f $f
      switch $f
        case '*zip'
          echo "$f already zipped"
        case '*'
          zip -9 $f.zip $f
          rm $f
       end
    end
  end
end

function backup
  cd /opt/work/backup
  if count $argv > /dev/null
    cd $argv
  end
end

function home_pr
  cd /opt/work/home
end

function group_fix
  sudo_run grpck
  sudo_run pwck
end

## Aliases
function ls --description 'List contents of directory'
  set -l param --color=auto
  if isatty 1
    set param $param --indicator-style=classify
  end
  if [ (uname -o) = "GNU/Linux" ]
    set param $param --group-directories-first
  end
  command ls $param $argv
end

function l
  ls -CF $argv
end

function la
  ls -A $argv
end

function ll
  ls -ahlF $argv
end

function hm -d "Merge history from several shells"
  history --merge
end

function prj -d "project directory"
  # if argv when go to directory
  set -l path $MY_PROJECTS_ROOT
  if count $argv > /dev/null
    set -x path $path"/"$argv
  end
  cd $path
end
complete -c prj -a '(__fish_complete_directories (eval $MY_PROJECTS_ROOT))'

function rmv
  sudo_run mv $argv /tmp
end

function soff
  sudo_run swapoff (swapon --noheadings --show=NAME) # /dev/sda4
end

function son
  sudo_run swapon (swapon --noheadings --show=NAME) #/dev/mapper/xubuntu--vg-swap_1
end

function systemctl
  sudo_run systemctl $argv
end

if type -pq tmux
  function tm
    tmux attach
    tmux new
  end
end

if type -pq emacs
  set -x EDITOR "emacs -nw"
  function em
    emacs -nw $argv
  end

  function sem
    sudo_run emacs -nw $argv
  end
else if type -pq vim
  set -x EDITOR vim
end

if type -pq mc
  function smc
    sudo_run -E mc
  end
end

function _list_git_branches
    # In some cases, git can end up on no branch - e.g. with a detached head
    # This will result in output like `* (no branch)` or a localized `* (HEAD detached at SHA)`
    # The first `string match -v` filters it out because it's not useful as a branch argument
    command git branch --no-color -a $argv ^/dev/null | string match -v '\* (*)' | string match -r -v ' -> ' | string trim -c "* " | string replace -r "^remotes/" ""
end

if type -pq git
  function pll
    git pull origin $argv
  end
  complete -c pll -a '(_list_git_branches)' -d 'Pull from origin branches'
  function psh
    git push origin $argv
  end
  complete -c psh -a '(_list_git_branches)' -d 'Push to origin branches'
  function gst
    git status
  end
  function gco
    git checkout $argv
  end
  complete -f -c gco -a '(_list_git_branches)' -d 'Checkout branches'
  function gadd
    git add .
  end
  function gbr
    git branch $argv
  end
  complete -f -c gbr -a '(_list_git_branches)' -d 'Branch'
  function gcmt
    git commit -m "$argv"
  end
end

function update_kernel
  sudo_run mkinitcpio -p linux
end

if test -f $fish_config_path/local.fish
  source $fish_config_path/local.fish
end

# start X at login
#if status --is-login
#  if test -z "$DISPLAY" -a $XDG_VTNR -eq 1
#    exec /bin/bash startx -- -keeptty
#  end
#end

# if test -z "$DESKTOP_SESSION"
#   eval (gnome-keyring-daemon --start)
#   set -x SSH_AUTH_SOCK
# end


function fish_title
  echo $_ ' '
  echo (prompt_pwd)
end
