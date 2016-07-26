function fish_prompt --description 'Write out the prompt'

    set -l fish_color_status red
    set -l fish_color_cwd magenta
    set -l fish_color_venv yellow
    set -l fish_color_git $fish_color_normal
    set -l last_status $status
    set -l fish_color_user green
    set -l fish_color_root red
    set -l fish_color_date cyan

    if not set -q __fish_git_prompt_show_informative_status
        set -g __fish_git_prompt_show_informative_status 1
    end
    if not set -q __fish_git_prompt_hide_untrackedfiles
        set -g __fish_git_prompt_hide_untrackedfiles 1
    end

    if not set -q __fish_git_prompt_color_branch
        set -g __fish_git_prompt_color_branch cyan --bold
    end
    if not set -q __fish_git_prompt_showupstream
        set -g __fish_git_prompt_showupstream "informative"
    end
    if not set -q __fish_git_prompt_char_upstream_ahead
        set -g __fish_git_prompt_char_upstream_ahead "↑"
    end
    if not set -q __fish_git_prompt_char_upstream_behind
        set -g __fish_git_prompt_char_upstream_behind "↓"
    end
    if not set -q __fish_git_prompt_char_upstream_prefix
        set -g __fish_git_prompt_char_upstream_prefix ""
    end

    if not set -q __fish_git_prompt_char_stagedstate
        set -g __fish_git_prompt_char_stagedstate "●"
    end
    if not set -q __fish_git_prompt_char_dirtystate
        set -g __fish_git_prompt_char_dirtystate "✚"
    end
    if not set -q __fish_git_prompt_char_untrackedfiles
        set -g __fish_git_prompt_char_untrackedfiles "…"
    end
    if not set -q __fish_git_prompt_char_conflictedstate
        set -g __fish_git_prompt_char_conflictedstate "✖"
    end
    if not set -q __fish_git_prompt_char_cleanstate
        set -g __fish_git_prompt_char_cleanstate "✔"
    end

    if not set -q __fish_git_prompt_color_dirtystate
        set -g __fish_git_prompt_color_dirtystate yellow
    end
    if not set -q __fish_git_prompt_color_stagedstate
        set -g __fish_git_prompt_color_stagedstate green
    end
    if not set -q __fish_git_prompt_color_invalidstate
        set -g __fish_git_prompt_color_invalidstate red
    end
    if not set -q __fish_git_prompt_color_untrackedfiles
        set -g __fish_git_prompt_color_untrackedfiles $fish_color_normal
    end
    if not set -q __fish_git_prompt_color_cleanstate
        set -g __fish_git_prompt_color_cleanstate green --bold
    end

    if test $CMD_DURATION
        if test $CMD_DURATION -gt (math "1000 * 10")
          set -l secs (math "$CMD_DURATION / 1000")
          echo 
          set -g __fish_prompt_duration (set_color red) $secs "s"
        else 
          set -g __fish_prompt_duration ""
        end
    end

    switch $USER

      case root
        set -g __fish_prompt_user (set_color $fish_color_root) $USER
      case '*'
        set -g __fish_prompt_user (set_color $fish_color_user) $USER
    end

    set -g __date (set_color $fish_color_date) (date "+%H:%M")

    set -l delim '➤ '

    set -g __fish_prompt_cwd (set_color $fish_color_cwd) "{"(prompt_pwd)"}"

    set -l prompt_status

    if test $last_status -ne 0
        if not set -q __fish_prompt_status
            set -g __fish_prompt_status (set_color $fish_color_status)
        end
        set prompt_status "$__fish_prompt_status [$last_status]"
    end

    if set -q VIRTUAL_ENV
        set -g virtual_env (set_color $fish_color_venv) "["(basename "$VIRTUAL_ENV")"]"
    else
        set -g virtual_env ""
    end

    set -g __fish_prompt_git ""
    set -l git_branch (__fish_git_prompt)
    if test $git_branch
        set -g __fish_prompt_git (set_color $fish_color_git) (__fish_git_prompt) (__fish_git_prompt_informative_status)
    end

    echo -n -s "$__date" "$virtual_env" "$__fish_prompt_user"  "$__fish_prompt_cwd" "$__fish_prompt_git" "$prompt_status" "$__fish_prompt_duration"
    printf "\n"
    echo -n (set_color $fish_color_user)"$delim"
end
