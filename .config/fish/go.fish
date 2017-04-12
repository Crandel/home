set -l GOPATH1 $HOME/go
set -xg MY_GO_PROJECTS_ROOT $GOPATH1/goprojects
set -xg GOPATH $GOPATH1:$MY_GO_PROJECTS_ROOT

set -xg PATH $PATH $GOPATH1/bin
set -xg GO_VENDOR 1

function go_path
	cd ~/go
end
