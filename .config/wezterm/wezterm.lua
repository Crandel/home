local wezterm = require 'wezterm';

wezterm.on('update-right-status', function(window, pane)
  local date = wezterm.strftime '%d-%m-%Y %H:%M:%S'
  local cwd_uri = pane:get_current_working_dir()
  cwd_uri = cwd_uri:sub(8)
  local slash = cwd_uri:find '/'
  cwd = cwd_uri:sub(slash)
  window:set_right_status(wezterm.format {
    { Text = cwd .. ' | ' .. date},
  })
end)

wezterm.on("toggle-opacity", function(window, pane)
  local overrides = window:get_config_overrides() or {}
  if not overrides.window_background_opacity then
    overrides.window_background_opacity = 0.9;
  else
    overrides.window_background_opacity = nil
  end
  window:set_config_overrides(overrides)
end)

return {
  check_for_updates = false,
  color_scheme = "Gruvbox Dark Hard",
  color_schemes = {
    ["Gruvbox Dark Hard"] = {
      foreground = "#E6D4A3",
      background = "#1E1E1E",
      cursor_bg = "#EBDBB2",
      cursor_border = "#EBDBB2",
      cursor_fg = "#FFDDCC",
      selection_bg = "#EBDBB2",
      selection_fg = "#333333",

      ansi = {"#282828","#CC241D","#98971A","#D79921","#458588","#B16286","#689D6A","#A89984"},
      brights = {"#928374","#FB4934","#B8BB26","#FABD2F","#83A598","#D3869B","#8EC07C","#EBDBB2"},
      indexed = {
        [24]  = "#076678", [66]  = "#458588", [72]  = "#689D6A", [88]  = "#9D0006", [96]  = "#8F3F71",
        [100] = "#79740E", [106] = "#98971A", [108] = "#8EC07C", [109] = "#83A598", [124] = "#CC241D",
        [130] = "#AF3A03", [132] = "#B16286", [136] = "#B57614", [142] = "#B8BB26", [166] = "#D65D0E",
        [167] = "#FB4934", [172] = "#D79921", [175] = "#D3869B", [208] = "#FE8019", [214] = "#FABD2F",
        [223] = "#EBDBB2", [228] = "#F2E5BC", [229] = "#FBF1C7", [230] = "#F9F5D7", [234] = "#1D2021",
        [235] = "#282828", [236] = "#32302F", [237] = "#3C3836", [239] = "#504945", [241] = "#665C54",
        [243] = "#7C6F64", [244] = "#928374", [245] = "#928374", [246] = "#A89984", [248] = "#BDAE93",
        [250] = "#D5C4A1"
      }
    }
  },
  colors = {
    tab_bar = {
      -- The active tab is the one that has focus in the window
      active_tab = {
        -- The color of the background area for the tab
        bg_color = "#AF8700",
        -- The color of the text for the tab
        fg_color = "#EBDBB2",
        -- Specify whether you want "Half", "Normal" or "Bold" intensity for the
        -- label shown for this tab.
        -- The default is "Normal"
        intensity = "Normal",
        -- Specify whether you want "None", "Single" or "Double" underline for
        -- label shown for this tab.
        -- The default is "None"
        underline = "None",

        -- Specify whether you want the text to be italic (true) or not (false)
        -- for this tab.  The default is false.
        italic = false,

        -- Specify whether you want the text to be rendered with strikethrough (true)
        -- or not for this tab.  The default is false.
        strikethrough = false,
      },
      -- The color of the strip that goes along the top of the window
      background = "#1D2021",
      -- Inactive tabs are the tabs that do not have focus
      inactive_tab = {
        bg_color = "#181907",
        fg_color = "#E6D4A3",

        -- The same options that were listed under the `active_tab` section above
        -- can also be used for `inactive_tab`.
      },

      -- You can configure some alternate styling when the mouse pointer
      -- moves over inactive tabs
      inactive_tab_hover = {
        bg_color = "#282828",
        fg_color = "#E6D4A3",
        italic = true,
      }
    }
  },
  cursor_blink_rate = 1000,
  default_cursor_style = "BlinkingBar",
  enable_scroll_bar = true,
  enable_wayland = true,
  font = wezterm.font_with_fallback({"Hack Nerd Font Mono","Hack"}),
  font_size = 20.0,
  hide_tab_bar_if_only_one_tab = true,
  keys = {
    {key="a"          ,mods="CTRL|SHIFT" ,action="ActivateCopyMode"},
    {key="b"          ,mods="CTRL|SHIFT" ,action=wezterm.action{EmitEvent="toggle-opacity"}},
    {key="DownArrow"  ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Down"}},
    {key="LeftArrow"  ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="RightArrow" ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Right"}},
    {key="UpArrow"    ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Up"}},
    {key="UpArrow"    ,mods="CTRL|SHIFT" ,action=wezterm.action{ScrollByPage=-1}},
    {key="DownArrow"  ,mods="CTRL|SHIFT" ,action=wezterm.action{ScrollByPage=1}},
    {key="Tab"        ,mods="CTRL"       ,action=wezterm.action{ActivateTabRelative=1}},
    {key="Tab"        ,mods="CTRL|SHIFT" ,action=wezterm.action{ActivateTabRelative=-1}},
    {key="x"          ,mods="CTRL|SHIFT" ,action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
    {key="z"          ,mods="CTRL|SHIFT" ,action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
    {key="t"          ,mods="CTRL"       ,action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
    {key="w"          ,mods="CTRL"       ,action=wezterm.action{CloseCurrentPane={confirm=true}}},
    {key="z"          ,mods="CTRL"       ,action="Nop"},
    {key="z"          ,mods="ALT"        ,action="TogglePaneZoomState"},
  },
  ratelimit_mux_line_prefetches_per_second = 4289999998,
  scrollback_lines = 150000,
  tab_bar_at_bottom = true,
  tab_max_width = 46,
  text_background_opacity = 1.0,
  use_fancy_tab_bar = true,
  warn_about_missing_glyphs = false,
  window_background_opacity = 1.0,
  window_frame = {
    -- The font used in the tab bar.
    font = wezterm.font_with_fallback({"Hack Nerd Font Mono","Hack"}),
    font_size = 16.0,
    -- The overall background color of the tab bar when
    -- the window is focused
    active_titlebar_bg = '#333333',
    -- The overall background color of the tab bar when
    -- the window is not focused
    inactive_titlebar_bg = '#333333',
  },
  window_padding = {
    left = 0,
    right = 2,
    top = 0,
    bottom = 0,
  },
}
