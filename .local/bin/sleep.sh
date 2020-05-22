#! /bin/bash

if ! pgrep "sway" ; then
  if hash i3lock >/dev/null 2>&1 && ! pgrep "i3lock" > /dev/null ; then
    dbus-send --type=method_call --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.set_layout uint32:2
    echo "Succesfully block screen using i3lock"
    i3lock -c 000000 -n -e -f
    echo "i3lock was closed"
  fi
else
  if hash swaylock >/dev/null 2>&1 && ! pgrep "swaylock" > /dev/null ; then
    swaymsg input type:keyboard xkb_switch_layout 0
    echo "Succesfully block screen using swaylock"
    swaylock -c 000000 -F -e -k -l --font Hack --font-size 22 -i ~/Pictures/wallpaper.jpg
    echo "swaylock was closed"
  fi
fi
