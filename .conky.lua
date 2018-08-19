conky.config = {
    out_to_x = false,
    out_to_console = true,
    own_window = false,
    background = false,
    max_text_width = 0,
    -- Update interval in seconds
    update_interval = 3.0,
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
    -- template0 = [[${exec pactl list sinks | grep 'Volume: front-left' -m 1 | awk '{print $5}'}]],
    -- Volume on/off
    -- template1 = [[${exec pactl list sinks | grep 'Mute:' -m 1 | awk '{print $2}'}]],
    template0 = [[${exec gmail}]],
};
-- Stuff in text will be formatted on screen
-- JSON for i3bar
conky.text = [[
[
{ "full_text" : "\uF0E0 $template0", "color" : 
  ${if_match "$template0" == "C:0"}
    "\#2E64FE"
  ${else}
    "\#D0FA58"
  ${endif}
},
{ "full_text" : "\uF07C /${fs_free /}|h${fs_free /home}|d${fs_free /media/data}", "color" : "\#FA5882" },
{ "full_text": "\uF1DE ${cpubar}", "color" :
  ${if_match ${cpu}<90}
    "\#04B404"
  ${else}
    "\#FF0000"
  ${endif}
},
{ "full_text" : "\uF0F3$pa_sink_volume%", "color": 
${if_pa_sink_muted}
"\#FF0000"
${else}
"\#E1F5A9"
${endif}
},
{ "full_text" : "\uF1C0$mem" , "color" :
${if_match ${memperc}<90}
"\#F7FE2E"
${else}
"\#FF0000"
${endif}
},
{ "full_text" : "\uF17E $swapperc", "color" : "\#FE9A2E"},
{ "full_text" : "\uF205 $uptime_short", "color": "\#8B008B"},
{ "full_text" : "\uF241 $battery_short", "color" : "\#FE9A2E"},
${if_up enp9s0}
{ "full_text" : "\uF0E4 \u2193${downspeed enp9s0}", "color": "\#F6CECE"},
${endif}
${if_up wlp8s0}
{ "full_text" : "\uF1EB \u21CA${downspeed wlp8s0}|\u21C8${upspeed wlp8s0}|${wireless_link_qual_perc wlp8s0}|$wireless_essid", "color": "\#F6CECE"},
${endif}
{ "full_text" : "${time  %d/%m/%y %H:%M}", "color" : "\#00FF40"}],
]];
