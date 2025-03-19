#!/bin/sh

prompt="$1"
shift

fuzzel.sh -p "$prompt" "$@"
