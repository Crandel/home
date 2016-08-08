conky.config = {
    out_to_x = false,
    out_to_console = true,
    own_window = false,
    background = false,
    max_text_width = 0,
    -- Update interval in seconds
    update_interval = 2.0,
    --[[
        This is the number of times Conky will update before quitting.
        Set to zero to run forever. ]]
    total_run_times = 0,
    -- Shortens units to a single character (kiB->k, GiB->G, etc.). Default is off.
    short_units = true,
    --[[
        How strict should if_up be when testing an interface for being up?
        The value is one of up, link or address, to check for the interface
        being solely up, being up and having link or being up, having link
        and an assigned IP address.]]
    if_up_strictness = "address",
    --[[
        Add spaces to keep things from moving about?  This only affects certain objects.
        use_spacer should have an argument of left, right, or none ]]
    use_spacer = nil,
    --[[
         Force UTF8? note that UTF8 support required XFT ]]
    override_utf8_locale = false,
    --[[
        number of cpu samples to average
        set to 1 to disable averaging ]]
    cpu_avg_samples = 1,
    top_name_width = 8,
    -- Volume level
    template0 = [[${exec amixer -c 0 get Master | grep Mono: | awk '{print $4}' | tr -d '[]'}]],
    -- Volume on/off
    template1 = [[${exec amixer -c 0 get Master | grep Mono: | awk '{print $6}' | tr -d '[]'}]],
};
-- Stuff in text will be formatted on screen
-- JSON for i3bar
conky.text = [[
[
{ "full_text" : "\uEB05 ${exec gmail}", "color" : "\#2E64FE"},
{ "full_text" : "\u26C1 /${fs_free /}|h${fs_free /home}|d${fs_free /media/data}", "color" : "\#FA5882" },
{ "full_text": "\uE003 ${cpu cpu1}%, ${cpu cpu2}%, ${cpu cpu3}%, ${cpu cpu4}%", "color" :
  ${if_match ${cpu}<90}
    "\#04B404"
  ${else}
    "\#FF0000"
  ${endif}
},
${if_match ${cpu}>90}
{ "full_text" : "T ${top name 1}", "color" : "\#F0E68C", "separator_block_width": 1},
${endif}
${if_match ${memperc}>90}
{ "full_text" : "T ${top name 1}", "color" : "\#F0E68C", "separator_block_width": 1},
${endif}
{ "full_text" : "\u266C $template0", "color":
  ${if_match "$template1"=="on"}
    "\#E1F5A9"
  ${else}
    "\#FF0000"
  ${endif}
},
{ "full_text" : "\u2622 $mem" , "color" :
  ${if_match ${memperc}<90}
    "\#F7FE2E"
  ${else}
    "\#FF0000"
  ${endif}
},
${if_up enp3s0}
{ "full_text" : "\u2614 $swapperc", "color" : "\#FE9A2E"},
{ "full_text" : "\u2735 $uptime_short", "color": "\#8B008B"},
{ "full_text" : "\u2042 \u2193${downspeed enp3s0}", "color": "\#F6CECE"},
${endif}
${if_up wlp3s0}
{ "full_text" : "\u267B $battery_short", "color" : "\#FE9A2E"},
{ "full_text" : "\u26B6 \u2193${downspeed wlp3s0}|${wireless_link_qual_perc wlp3s0}|$wireless_essid", "color": "\#F6CECE"},
${endif}
{ "full_text" : "${time  %d/%m/%y %H:%M}", "color" : "\#00FF40"}],
]];
