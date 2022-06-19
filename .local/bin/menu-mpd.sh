#!/bin/bash

MENU=${MENU:-fuzzel-dmenu}

prompt="mpc:"
prompt_add="mpc-add:"

cmd_list="play stop next prev toggle add volume seek repeat single consume random clear crop"

mpc_add() {
    file="$(mpc listall | ${MENU} -l 15 -P "$prompt_add")"
    [[ -z $file ]] && exit 1
    mpc add "$file"
    mpc play
}

cmd="$(echo $cmd_list | sed 's/ /\n/g' | ${MENU} -l 20 -P "$prompt")"
[[ -z $cmd ]] && exit 1

if [[ $cmd = add ]]; then
    mpc_add
else
    mpc $cmd
fi
