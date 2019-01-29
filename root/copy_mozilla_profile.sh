#!/bin/sh

mozilla_profile_dir="/data/data/org.mozilla.firefox/files/mozilla/*.default"
user_file="/storage/emulated/0/progs/termux/home/termux/user.js"
chrome="/storage/emulated/0/progs/termux/home/termux/chrome"

cd $mozilla_profile_dir
echo "$pwd"
cp -R $user_file .
cp -R $chrome .
usernme=$(stat -c "%U" .)
grp=$(stat -c "%G" .)
echo "$(ls -la | grep --color=auto user.js)"
chown -R "$usernme:$grp" .
echo "$(ls -la | grep  --color=auto user.js)"

