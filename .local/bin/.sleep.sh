#! /bin/bash
# if hash i3lock >/dev/null 2>&1 && ! pgrep "i3lock" > /dev/null ; then
#   dbus-send --type=method_call --dest=ru.gentoo.KbddService /ru/gentoo/KbddService ru.gentoo.kbdd.set_layout uint32:2
#   echo "Succesfully block screen"
#   i3lock -c 000000 -n
#   echo "i3lock was closed"
# fi
if hash swaylock >/dev/null 2>&1 && ! pgrep "swaylock" > /dev/null ; then
  echo "Succesfully block screen"
  swaylock -c 000000 -n
  echo "i3lock was closed"
fi
