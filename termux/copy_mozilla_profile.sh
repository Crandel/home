#!/bin/bash

folder_list=(
  "org.mozilla.fenix"
  "org.mozilla.firefox"
  "org.mozilla.fennec_fdroid"
  "org.mozilla.firefox_beta"
)

cd termux/firefox
cwd=$(pwd)
ls -lA $cwd

function update_settings () {
  folder=$1
  full_path=/data/data/$folder/files/mozilla/*.default
  if [ -d $full_path ]; then
    cd $full_path
    echo "Current dir is $(pwd)\n"
    usernme=$(stat -c "%U" .)
    grp=$(stat -c "%G" .)
    cp -r $cwd/* .
    echo "Before chown $(ls -lA | grep -i user)\n"
    chown -R "$usernme:$grp" .
    echo "After chown $(ls -lA | grep -i user)\n"
  fi
}

for folder in "${folder_list[@]}"; do
  echo "Current folder is $folder"
  update_settings $folder
done

cd $cwd
