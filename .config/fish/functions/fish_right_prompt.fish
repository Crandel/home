function fish_right_prompt
    set -l fish_color_user green 
    set -l fish_color_root red
    set -l fish_color_date cyan

    if test $status = 0
        echo -n (set_color green)
    else
        echo -n (set_color red)
    end

    switch $USER

    case root
        set -g __fish_prompt_user (set_color $fish_color_root)
    case '*'
        set -g __fish_prompt_user (set_color $fish_color_user)
    end

    set -g __date (set_color $fish_color_date) (date "+%H:%M")
    echo "$CMD_DURATION"(set_color normal)ms "$__fish_prompt_user" "$USER" "$__date" (set_color normal)
end

