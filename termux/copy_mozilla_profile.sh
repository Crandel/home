#!/bin/bash

folder_list=("org.mozilla.firefox" "org.mozilla.fenix" "org.mozilla.fenix.nightly" "org.mozilla.fennec_fdroid")

cd termux/firefox
cwd=$(pwd)
echo "PWD is $(pwd)"
echo "BASEDIR is $cwd"


function update_settings () {
  folder=$1
  full_path=/data/data/$folder/files/mozilla/*.default
  if [ -d $full_path ]; then
    echo "Current dir is $(pwd)"
    cd $full_path
    usernme=$(stat -c "%U" .)
    grp=$(stat -c "%G" .)
    cp -r $cwd/* .
    echo "$(ls -lA | grep -i user*)"
    chown -R "$usernme:$grp" .
    echo "$(ls -lA | grep -i user*)"
  fi
}

for folder in "${folder_list[@]}"; do
  echo "$folder"
  update_settings $folder
done
