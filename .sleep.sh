#! /bin/bash
if ! pgrep "i3lock" > /dev/null ; then
  i3lock -c 000000 -n
fi
