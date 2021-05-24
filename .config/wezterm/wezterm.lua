local wezterm = require 'wezterm';
return {
   color_scheme = "Gruvbox Dark",
   enable_scroll_bar = true,
   enable_wayland = true,
   font = wezterm.font_with_fallback({"Hack Nerd Font Mono","Hack"}),
   font_size=18.0,
   scrollback_lines = 150000,
}
