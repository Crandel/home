set -l GOPATH1 $HOME/go
if test -d $GOPATH1/goprojects
    set -xg MY_GO_PROJECTS_ROOT $GOPATH1/goprojects
    if test -d $MY_GO_PROJECTS_ROOT/src
        set -l glistpr "(ls $MY_GO_PROJECTS_ROOT/src | string replace '/' \t)"
        function gpr
            # if argv when go to directory
            set -l path $MY_GO_PROJECTS_ROOT
            if count $argv > /dev/null
                set -x path $path"/src/"$argv
                # switch (echo $argv)
                #   case go_chat
                #     set -l check (docker inspect -f "{{.State.Running}}" postgres)
                #     if [ $check = "false" ]
                #       docker start postgres
                #     end
                # end
            end
            cd $path
        end
        complete -c gpr -a "(__fish_complete_directories (eval $MY_GO_PROJECTS_ROOT\"/src\"))"
    end
end
set -xg GOPATH $GOPATH1:$MY_GO_PROJECTS_ROOT

set -xg PATH $PATH $GOPATH1/bin
set -xg GO_VENDOR 1

function go_path
    cd ~/go
end
