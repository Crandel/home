#!/bin/sh

folder_list=("org.mozilla.firefox" "org.mozilla.fenix" "org.mozilla.fennec_fdroid")

cd termux/firefox
cwd=$(pwd)
echo "PWD is $pwd"
echo "BASEDIR is $cwd"


function update_settings () {
  folder=$1
  echo "$folder"
  cd /data/data/$folder/files/mozilla/*.default
  pwd
  echo "$(ls -la | grep -i user*)"
  echo ""
  cp -r $cwd/* .
  usernme=$(stat -c "%U" .)
  grp=$(stat -c "%G" .)
  chown -R "$usernme:$grp" .
  echo "$(ls -la | grep -i user*)"
}

for folder in "${folder_list[@]}"; do
  if [ -d $folder]; then
    update_settings $folder
  fi
done
