if [ -z "$DISPLAY" ] && [ -n "$XDG_VTNR" ] && [ "$XDG_VTNR" -eq 1 ]; then
  # Possible arguments sway|i3|xfce
  exec ~/.local/bin/initrc sway
fi
