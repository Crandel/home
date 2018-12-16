#!/bin/sh

mozilla_profile_dir="/data/data/org.mozilla.firefox/files/mozilla/*.default"
user_file="/storage/emulated/0/progs/termux/home/termux/user.js"

cd $mozilla_profile_dir
echo "$pwd"
cp $user_file .
usernme=$(stat -c "%U" .)
grp=$(stat -c "%G" .)
echo "$(ls -la | grep --color=auto user.js)"
chown "$usernme:$grp" user.js
echo "$(ls -la | grep  --color=auto user.js)"

