#! /bin/bash
if [ ! $(pgrep "i3lock" > /dev/null) ]; then
    killall -SIGUSR2 compton
    i3lock -c 000000 -n
    compton --config ~/.config/compton.conf -b
fi
