function projects
    # if argv when go to directory
    if count $argv > /dev/null
        cd $MY_PROJECTS_ROOT/$argv
    else
        cd $MY_PROJECTS_ROOT
    end
end

