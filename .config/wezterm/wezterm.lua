local wezterm = require 'wezterm';

local act = wezterm.action

-- This table will hold the configuration.
local config = {}

-- In newer versions of wezterm, use the config_builder which will
-- help provide clearer error messages
if wezterm.config_builder then
  config = wezterm.config_builder()
end

wezterm.on('update-status', function(window, pane)
             local date = wezterm.strftime '%d-%m-%Y %H:%M:%S'
             window:set_right_status(wezterm.format {
                                       { Text = date},
             })
end)

wezterm.on("toggle-opacity", function(window, pane)
             local overrides = window:get_config_overrides() or {}
             if not overrides.window_background_opacity then
               overrides.window_background_opacity = 0.99;
             else
               overrides.window_background_opacity = nil
             end
             window:set_config_overrides(overrides)
end)

config.check_for_updates = false

config.color_scheme = "GruvboxDarkHard"

config.colors = {
  tab_bar = {
    -- The active tab is the one that has focus in the window
    active_tab = {
      -- The color of the background area for the tab
      bg_color = "#D65D0E",
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
    background = "#282828",
    -- Inactive tabs are the tabs that do not have focus
    inactive_tab = {
      bg_color = "#282828",
      fg_color = "#FADB2F",

      -- The same options that were listed under the `active_tab` section above
      -- can also be used for `inactive_tab`.
    },
  }
}

config.cursor_blink_rate = 1000

config.default_cursor_style = "BlinkingBar"

config.enable_scroll_bar = true

config.enable_wayland = true

-- config.front_end = "WebGpu"

config.font = wezterm.font_with_fallback({"FiraCode Nerd Font Mono","Hack Nerd Font"})

config.font_size = 22

config.hide_tab_bar_if_only_one_tab = true

config.hyperlink_rules = {
  -- Linkify things that look like URLs and the host has a TLD name.
  -- Compiled-in default. Used if you don't specify any hyperlink_rules.
  {
    regex = '\\b\\w+://[\\w.-]+\\.[a-z]{2,15}\\S*\\b',
    format = '$0',
  },
  -- linkify email addresses
  -- Compiled-in default. Used if you don't specify any hyperlink_rules.
  {
    regex = [[\b\w+@[\w-]+(\.[\w-]+)+\b]],
    format = 'mailto:$0',
  },

  -- file:// URI
  -- Compiled-in default. Used if you don't specify any hyperlink_rules.
  {
    regex = [[\bfile://\S*\b]],
    format = '$0',
  },

  -- Linkify things that look like URLs with numeric addresses as hosts.
  -- E.g. http://127.0.0.1:8000 for a local development server,
  -- or http://192.168.1.1 for the web interface of many routers.
  {
    regex = [[\b\w+://(?:[\d]{1,3}\.){3}[\d]{1,3}\S*\b]],
    format = '$0',
  },

  -- Linkify things that look like URLs with localhost.
  -- E.g. http://localhost:8000 for a local development server,
  {
    regex = [[\b\w+://(?:[\w.-]+)(?:(:?:\.[a-z]{2,15}\S*)|(?::\d{1,5}))\b]],
    format = '$0',
  },
}

config.keys = {
  {key="a"          ,mods="CTRL|SHIFT" ,action=act.ActivateCopyMode },
  {key="b"          ,mods="CTRL|SHIFT" ,action=act.EmitEvent "toggle-opacity" },
  {key="DownArrow"  ,mods="ALT"        ,action=act.ActivatePaneDirection "Down" },
  {key="LeftArrow"  ,mods="ALT"        ,action=act.ActivatePaneDirection "Left" },
  {key="RightArrow" ,mods="ALT"        ,action=act.ActivatePaneDirection "Right" },
  {key="UpArrow"    ,mods="ALT"        ,action=act.ActivatePaneDirection "Up" },
  {key="UpArrow"    ,mods="CTRL|SHIFT" ,action=act.ScrollByPage(-1) },
  {key="DownArrow"  ,mods="CTRL|SHIFT" ,action=act.ScrollByPage(1) },
  {key="Tab"        ,mods="CTRL"       ,action=act.ActivateTabRelative(1) },
  {key="Tab"        ,mods="CTRL|SHIFT" ,action=act.ActivateTabRelative(-1) },
  {key="PageUp"     ,mods="SHIFT"      ,action=act.ScrollByPage(-0.5) },
  {key="PageDown"   ,mods="SHIFT"      ,action=act.ScrollByPage(0.5) },
  {key="x"          ,mods="CTRL|SHIFT" ,action=act.SplitHorizontal { domain = "CurrentPaneDomain" } },
  {key="z"          ,mods="CTRL|SHIFT" ,action=act.SplitVertical { domain = "CurrentPaneDomain" } },
  {key="t"          ,mods="CTRL"       ,action=act.SpawnTab "CurrentPaneDomain" },
  {key="w"          ,mods="CTRL"       ,action=act.CloseCurrentPane { confirm = true } },
  {key="z"          ,mods="ALT"        ,action=act.TogglePaneZoomState },
  {key=","          ,mods="CTRL|ALT"   ,action=act.DecreaseFontSize },
  {key="."          ,mods="CTRL|ALT"   ,action=act.IncreaseFontSize },
  -- Disable standart annoying keybindings
  {key="z"          ,mods="CTRL"       ,action=act.DisableDefaultAssignment },
  {key="-"          ,mods="CTRL|SHIFT" ,action=act.DisableDefaultAssignment },
  {key="-"          ,mods="CTRL"       ,action=act.DisableDefaultAssignment },
  {key="-"          ,mods="SUPER"      ,action=act.DisableDefaultAssignment },
  {key="_"          ,mods="CTRL|SHIFT" ,action=act.DisableDefaultAssignment },
  {key="_"          ,mods="CTRL"       ,action=act.DisableDefaultAssignment },
  {key="="          ,mods="CTRL|SHIFT" ,action=act.DisableDefaultAssignment },
  {key="="          ,mods="CTRL"       ,action=act.DisableDefaultAssignment },
  {key="="          ,mods="SUPER"      ,action=act.DisableDefaultAssignment },
  {key="+"          ,mods="CTRL|SHIFT" ,action=act.DisableDefaultAssignment },
  {key="+"          ,mods="CTRL"       ,action=act.DisableDefaultAssignment },
  {key="Backspace"  ,mods="CTRL"       ,action=act.DisableDefaultAssignment },
}

config.ratelimit_mux_line_prefetches_per_second = 4289999998

config.scrollback_lines = 150000

config.mouse_bindings = {
  -- {
  --   event = { Down = { streak = 1, button = { WheelUp = 1 } } },
  --   mods = 'NONE',
  --   action = act.ScrollByPage(-0.5),
  -- },
  -- {
  --   event = { Down = { streak = 1, button = { WheelDown = 1 } } },
  --   mods = 'NONE',
  --   action = act.ScrollByPage(0.5),
  -- },
  {
    event = { Down = { streak = 1, button = { WheelUp = 1 } } },
    mods = 'CTRL',
    action = act.ScrollByLine(-3),
  },
  {
    event = { Down = { streak = 1, button = { WheelDown = 1 } } },
    mods = 'CTRL',
    action = act.ScrollByLine(3),
  },
}

config.tab_bar_at_bottom = true

config.tab_max_width = 46

config.text_background_opacity = 1.0

config.tiling_desktop_environments = {
  'X11 LG3D',
  'X11 bspwm',
  'X11 i3',
  'X11 dwm',
  'Wayland swaywm',
}

config.use_fancy_tab_bar = false

config.warn_about_missing_glyphs = false

config.window_background_opacity = 1.0

config.window_frame = {
  -- The font used in the tab bar.
  font = wezterm.font_with_fallback({"FiraCode Nerd Font Mono","Hack Nerd Font"}),
  font_size = 22,
  -- The overall background color of the tab bar when
  -- the window is focused
  active_titlebar_bg = '#B16286',
  -- The overall background color of the tab bar when
  -- the window is not focused
  inactive_titlebar_bg = '#1f2419',
}

config.window_padding = {
  left = 0,
  right = 2,
  top = 0,
  bottom = 0,
}

return config
