function ll
   ls -alF $argv
end
 
function la
   ls -A $argv
end
 
function l
   ls -CF $argv
end

function son
   sudo swapon /dev/sda5
end

function soff
    sudo swapoff /dev/sda5
end

function go_path
   cd ~/go
end

function tm
   tmux attach
   tmux new
end
