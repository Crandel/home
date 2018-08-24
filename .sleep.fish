#! /usr/bin/env fish
if not pgrep "i3lock" > /dev/null
    echo "i3locj"
    i3lock -c 000000
end
