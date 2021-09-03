local wezterm = require 'wezterm';

wezterm.on("toggle-opacity", function(window, pane)
  local overrides = window:get_config_overrides() or {}
  if not overrides.window_background_opacity then
    overrides.window_background_opacity = 1.0;
  else
    overrides.window_background_opacity = nil
  end
  window:set_config_overrides(overrides)
end)

return {
  colors = {
    -- The default text color
    foreground = "#E6D4A3",
    -- The default background color
    background = "#1E1E1E",

    -- Overrides the cell background color when the current cell is occupied by the
    -- cursor and the cursor style is set to Block
    cursor_bg = "#1E1E1E",
    -- Overrides the text color when the current cell is occupied by the cursor
    cursor_fg = "#FFDDCC",
    -- Specifies the border color of the cursor when the cursor style is set to Block,
    -- of the color of the vertical or horizontal bar when the cursor style is set to
    -- Bar or Underline.
    cursor_border = "#EBDBB2",
    cursor_blink_rate = 800,
    -- the foreground color of selected text
    selection_fg = "#333333",
    -- the background color of selected text
    selection_bg = "#EBDBB2",

    -- The color of the scrollbar "thumb"; the portion that represents the current viewport
    scrollbar_thumb = "#333333",

    -- The color of the split lines between panes
    split = "#444444",

    ansi = {"#1E1E1E","#BE0F17","#868715","#CC881A","#377375","#A04B73","#578E57","#978771"},
    brights = {"#7F7061","#F73028","#AAB01E","#F7B125","#719586","#C77089","#7DB669","#E6D4A3"},
    -- Arbitrary colors of the palette in the range from 16 to 255
    indexed = {[136] = "#AF8700"},

    tab_bar = {
      -- The active tab is the one that has focus in the window
      active_tab = {
        -- The color of the background area for the tab
        bg_color = "#AF8700",
        -- The color of the text for the tab
        fg_color = "#E6D4A3",
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
      background = "#1d2021",
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
  default_cursor_style = "BlinkingUnderline",
  enable_scroll_bar = true,
  enable_wayland = true,
  font = wezterm.font_with_fallback({"Hack Nerd Font Mono","Hack"}),
  font_size=18.0,
  hide_tab_bar_if_only_one_tab = true,
  keys = {
    {key="A"          ,mods="CTRL"       ,action="ActivateCopyMode"},
    {key="B"          ,mods="CTRL|SHIFT" ,action=wezterm.action{EmitEvent="toggle-opacity"}},
    {key="DownArrow"  ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Down"}},
    {key="LeftArrow"  ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Left"}},
    {key="RightArrow" ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Right"}},
    {key="Tab"        ,mods="CTRL"       ,action=wezterm.action{ActivateTabRelative=1}},
    {key="Tab"        ,mods="CTRL|SHIFT" ,action=wezterm.action{ActivateTabRelative=-1}},
    {key="UpArrow"    ,mods="ALT"        ,action=wezterm.action{ActivatePaneDirection="Up"}},
    {key="X"          ,mods="CTRL"       ,action=wezterm.action{SplitHorizontal={domain="CurrentPaneDomain"}}},
    {key="Z"          ,mods="CTRL"       ,action=wezterm.action{SplitVertical={domain="CurrentPaneDomain"}}},
    {key="t"          ,mods="CTRL"       ,action=wezterm.action{SpawnTab="CurrentPaneDomain"}},
    {key="w"          ,mods="CTRL"       ,action=wezterm.action{CloseCurrentPane={confirm=true}}},
    {key="z"          ,mods="CTRL"       ,action="Nop"},
  },
  ratelimit_mux_line_prefetches_per_second = 4289999998,
  scrollback_lines = 150000,
  tab_max_width = 46,
  text_background_opacity = 1.0,
  window_background_opacity = 0.9,
}
