#!/bin/bash
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)
if [ -d /etc/X11/xinit/xinitrc.d ]; then
  for f in /etc/X11/xinit/xinitrc.d/*; do
    [ -x "$f" ] && . "$f"
  done
  unset f
fi

source /etc/profile
export GNOME_DESKTOP_SESSION_ID=1
# exec gnome-session
# exec startkde
eval $(/usr/bin/gnome-keyring-daemon --start --components=pkcs11,secrets,ssh)
export SSH_AUTH_SOCK
exec i3 --shmlog-size=26214400
start-pulseaudio-x11
# ...or the Window Manager of your choice
