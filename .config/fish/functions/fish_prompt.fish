function fish_prompt --description 'Write out the prompt'
	# save to temp var because new command will override $status
	set -l last_status $status

	set -l fish_color_status red
	set -l fish_color_cwd magenta
	set -l fish_color_venv yellow
	set -l fish_color_user green
	set -l fish_color_root red
	set -l fish_color_delim green
	set -l fish_color_host white
	set -l fish_color_date cyan
	set -l fish_color_git_branch cyan
	set -l fish_color_git_unstaged yellow
	set -l fish_color_git_staged green
	set -l fish_color_git_ahead 7FFF00	# light green
	set -l fish_color_git_behind FF4500 # light red

	if test $CMD_DURATION
		if test $CMD_DURATION -gt (math "1000 * 10")
			set -l secs (math "$CMD_DURATION / 1000")
			echo
			set -g __duration (set_color red) $secs "s"(set_color $fish_color_normal)
		else
			set -g __duration ""
		end
	end

	switch $USER
		case root
			set -g __user (set_color $fish_color_root)$USER(set_color $fish_color_normal)
		case '*'
			set -g __user (set_color $fish_color_user)$USER(set_color $fish_color_normal)
	end
	set -g __user $__user@(set_color $fish_color_host)(hostname)(set_color $fish_color_normal)

	set -g __date (set_color $fish_color_delim)(date "+%H:%M")(set_color $fish_color_normal)

	set -g __pwd (set_color $fish_color_cwd)"{"(prompt_pwd)"}"(set_color $fish_color_normal)

	set -l __status

	if test $last_status -ne 0
		set __status (set_color $fish_color_status)" [$last_status]"(set_color $fish_color_normal)
		set fish_color_delim red
	end

	set -l __delim (set_color $fish_color_delim)'➤ '(set_color $fish_color_normal)

	if set -q VIRTUAL_ENV
		set -g __v_env (set_color $fish_color_venv)" ["(basename "$VIRTUAL_ENV")"]"(set_color $fish_color_normal)
	else
		set -g __v_env ""
	end

	set git_branch (git branch ^ /dev/null | sed -n 's/^\* //p')
	set -g __prompt_git ""
	if test $git_branch
		set oldIFS "$IFS"
		set IFS ""
		set -l new_status git status --porcelain
		set -l X (eval $new_status | cut -c 1-1)
		set -l Y (eval $new_status | cut -c 2-2)
		set -l modified_unstaged (echo $Y | grep "M" -c)
		set -l deleted_unstaged (echo $Y | grep "D" -c)
		set -l untracked_unstaged (echo $Y | grep "?" -c)
		set -l modified_staged (echo $X | grep "M" -c)
		set -l deleted_staged (echo $X | grep "D" -c)
		set -l renamed_staged (echo $X | grep "R" -c)
		set -l new_staged (echo $X | grep "A" -c)
		set -l ahead (git status -sb ^ /dev/null | grep -o "ahead [0-9]*" | grep -o "[0-9]*")
		set -l behind (git status -sb ^ /dev/null | grep -o "behind [0-9]*" | grep -o "[0-9]*")
		# unstagedFiles
		set unstagedFiles ""
		if test $modified_unstaged -ne 0
			set unstagedFiles "%%$modified_unstaged$unstagedFiles"
		end
		if test $untracked_unstaged -ne 0
			set unstagedFiles "*$untracked_unstaged$unstagedFiles"
		end
		if test $deleted_unstaged -ne 0
			set unstagedFiles "-$deleted_unstaged$unstagedFiles"
		end

		# stagedFiles
		set stagedFiles ""
		if test $modified_staged -ne 0
			set stagedFiles "%%$modified_staged$stagedFiles"
		end
		if test $deleted_staged -ne 0
			set stagedFiles "-$deleted_staged$stagedFiles"
		end
		if test $renamed_staged -ne 0
			set stagedFiles "^$renamed_staged$stagedFiles"
		end
		if test $new_staged -ne 0
			set stagedFiles "+$new_staged$stagedFiles"
		end

		set IFS "$oldIFS"

		set -g __git_branch (set_color $fish_color_git_branch)"$git_branch"(set_color $fish_color_normal)
		set -g __prompt_git (set_color $fish_color_normal)" ($__git_branch"
		if test $ahead
			set -l __ahead (set_color $fish_color_git_ahead)"{>$ahead}"(set_color $fish_color_normal)
			set -g __prompt_git "$__prompt_git$__ahead"
		end
		if test $behind
			set -l __behind (set_color $fish_color_git_behind)"{<$behind}"(set_color $fish_color_normal)
			set -g __prompt_git "$__prompt_git$__behind"
		end
		if test $unstagedFiles
			set -l __git_unstaged (set_color $fish_color_git_unstaged)"|$unstagedFiles"(set_color $fish_color_normal)
			set -g __prompt_git "$__prompt_git$__git_unstaged"
		end
		if test $stagedFiles
			set -l __git_staged (set_color $fish_color_git_staged)"|$stagedFiles"(set_color $fish_color_normal)
			set -g __prompt_git "$__prompt_git$__git_staged"
		end
		set -g __prompt_git "$__prompt_git)"
	end

	printf " $__date$__v_env $__user $__pwd$__prompt_git$__duration$__status\n$__delim"
end
