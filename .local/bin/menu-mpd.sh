#!/bin/bash

MENU=${MENU:-fuzzel-prompt.sh}

prompt="mpc:"
prompt_add="mpc-add:"

cmd_list="play stop next prev toggle add volume seek repeat single consume random clear crop"

mpc_add() {
    file="$(mpc listall | ${MENU} "$prompt_add" --dmenu -l 15 )"
    [[ -z $file ]] && exit 1
    mpc add "$file"
    mpc play
}

cmd="$(echo "$cmd_list" | sed 's/ /\n/g' | ${MENU} "$prompt" --dmenu -l 20 )"
[[ -z $cmd ]] && exit 1

if [[ $cmd = add ]]; then
    mpc_add
else
    mpc "$cmd"
fi
