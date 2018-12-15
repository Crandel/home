#!/bin/sh

mozilla_profile_dir="/data/data/org.mozilla.firefox/files/mozilla/*.default"
user_file="/storage/emulated/0/progs/termux/home/termux/user.js"

cd $mozilla_profile_dir
cp $user_file .
chown `$(stat -c "%U:%G")` user.js
