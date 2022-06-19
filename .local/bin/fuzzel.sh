#!/bin/sh

fuzzel \
  --dpi-aware=no \
  --font='Hack:weight=bold:size=16' \
  --background='1E1E1EFF' \
  --text-color='C0F440FF' \
  --match-color='83A598FF' \
  --selection-color='79740EFF' \
  --selection-text-color='EBDBB2FF' \
  --border-radius=20 \
  --terminal=alacritty \
  --icon-theme=Gruvbox-Dark \
  $@
