function d
	docker $argv
end

function dc
	if count $argv > /dev/null
		docker-compose $argv
	else
		docker-compose config
	end
end

function dst
  d stop (d ps -q)
end

function d_rm_all
  d rm (d ps -aq)
end

function run
	if count $argv > /dev/null
		set -l path $MY_PROJECTS_ROOT/$argv
		if test -d $path
			cd $path
		end
	end
	dc stop
	dc run --rm --service-ports app
end

function dl
	if count $argv > /dev/null
		dc logs --tail 15 $argv
	else
		dc logs --tail 15
	end
end
