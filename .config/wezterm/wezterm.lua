local wezterm = require 'wezterm';
return {
   color_scheme = "Gruvbox Dark",
   enable_scroll_bar = true,
   enable_wayland = true,
   font = wezterm.font_with_fallback({"Hack Nerd Font Mono","Hack"}),
   font_size=18.0,
   hide_tab_bar_if_only_one_tab = true,
   keys = {
      {key="Z", mods="CTRL", action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
      {key="X", mods="CTRL", action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
      {key="z", mods="CTRL", action="Nop"},
      {key="t", mods="CTRL", action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
      {key="A", mods="CTRL", action="ActivateCopyMode"}
   },
   ratelimit_output_bytes_per_second = 4289999998,
   scrollback_lines = 150000,
}
