#!/bin/sh

mozilla_profile_dir="/data/data/org.mozilla.firefox/files/mozilla/*.default"
user_files="/storage/emulated/0/progs/termux/home/termux/"

cd $mozilla_profile_dir
echo "$pwd"
cp -R $user_files .
usernme=$(stat -c "%U" .)
grp=$(stat -c "%G" .)
echo "$(ls -la | grep --color=auto user.js)"
chown -R "$usernme:$grp" .
echo "$(ls -la | grep  --color=auto user.js)"

