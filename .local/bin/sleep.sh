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
    echo "Swaylock with -s $SWAYSOCK beginning " >> /tmp/swaylock.log
    pkill swayidle
    pkill_exit=$?
    echo "Pkill swayidle: $pkill_exit"
    swaymsg -s $SWAYSOCK input type:keyboard xkb_switch_layout 0
    input_exit=$?
    echo "Exit code of swaymsg input switch: $input_exit" >> /tmp/swaylock.log
    echo "Turn screen on before swaylock" >> /tmp/swaylock.log
    swaymsg "output * dpms on"
    swaylock -c 000000 -F -e -k -l --font Hack --font-size 22
    lock_exit=$?
    echo "Succesfully block screen using swaylock: $lock_exit" >> /tmp/swaylock.log
    idle &
    idle_exit=$?
    echo "Run idle: $idle_exit" >> /tmp/swaylock.log
  fi
fi
