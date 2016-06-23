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

# exec gnome-session
# exec startkde
exec i3
start-pulseaudio-x11
# ...or the Window Manager of your choice
