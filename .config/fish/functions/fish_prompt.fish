function fish_prompt --description 'Write out the prompt'
    set -l fish_color_status red
    set -l fish_color_cwd magenta
    set -l fish_color_venv yellow
    set -l fish_color_git cyan
    set -l last_status $status
    set -l fish_color_user green 
    set -l fish_color_root red
    set -l fish_color_date cyan


    if test $CMD_DURATION
        if test $CMD_DURATION -gt (math "1000 * 10")
          set -l secs (math "$CMD_DURATION / 1000")
          echo 
          set -g __fish_prompt_duration (set_color red) $secs "s"
        else 
          set -g __fish_prompt_duration ""
        end
    end


    if not set -q -g __fish_classic_git_functions_defined
        set -g __fish_classic_git_functions_defined
        function __fish_repaint_user --on-variable fish_color_user --description "Event handler, repaint when fish_color_user changes"
            if status --is-interactive
                commandline -f repaint ^/dev/null
            end
        end
        
        function __fish_repaint_host --on-variable fish_color_host --description "Event handler, repaint when fish_color_host changes"
            if status --is-interactive
                set -e __fish_prompt_host
                commandline -f repaint ^/dev/null
            end
        end
        
        function __fish_repaint_status --on-variable fish_color_status --description "Event handler; repaint when fish_color_status changes"
            if status --is-interactive
                set -e __fish_prompt_status
                commandline -f repaint ^/dev/null
            end
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
