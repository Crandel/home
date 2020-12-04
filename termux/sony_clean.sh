#!/usr/bin/bash

declare -a list=(
  "com.amazon.amazonvideo.livingroom"
  "com.apple.atve.sony.appletv"
  "com.disney.disneyplus"
  "com.google.android.music"
  "com.google.android.play.games"
  "com.google.android.videos"
  "com.netflix.ninja"
  "com.netflix.partner.ncm"
  "com.qterics.da.product"
  "com.sony.dtv.airplayapp"
  "com.sony.dtv.demosupport"
  "com.sony.dtv.demosystemsupport"
  "tv.samba.ssm"
)

for app in "${list[@]}"
do
    echo "$app"
    adb shell pm uninstall -k --user 0 "$app"
    sleep 2
done
