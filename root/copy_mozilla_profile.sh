#!/bin/sh

set -e

firefox_profile_dir="/data/data/org.mozilla.firefox/files/mozilla/*.default"
fenix_profile_dir="/data/data/org.mozilla.fenix/files/mozilla/*.default"
user_files="../termux/firefox/*"

BASEDIR=$(dirname "$0")
echo "$pwd"
echo "$BASEDIR"
echo "$(ls -la $user_files)"

function update_settings () {
  folder=$1
  echo "$folder"
  cd $folder
  echo "$(ls -la | grep -i user*)"
  echo ""
  # cp -R $user_files .
  # usernme=$(stat -c "%U" .)
  # grp=$(stat -c "%G" .)
  # chown -R "$usernme:$grp" .
  # echo "$(ls -la)"
}

if [ -d $firefox_profile_dir ]; then
  update_settings $firefox_profile_dir
fi

if [ -d $fenix_profile_dir ]; then
  update_settings $fenix_profile_dir
fi


