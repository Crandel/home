#!/usr/bin/bash

declare -a list=(
"com.Slack"
"com.amazon.appmanager"
"com.android.chrome"
"com.coloros.childrenspace"
"com.coloros.filemanager"
"com.coloros.karaoke"
"com.coloros.ocrscanner"
"com.coloros.oshare"
"com.coloros.systemclone"
"com.coloros.video"
"com.coloros.weather.service"
"com.coloros.weather2"
"com.facebook.appmanager"
"com.facebook.services"
"com.facebook.system"
"com.google.ambient.streaming"
"com.google.android.adservices.api"
"com.google.android.apps.photos"
"com.google.android.apps.setupwizard.searchselector"
"com.google.android.apps.wellbeing"
"com.google.android.gm"
"com.google.android.odad"
"com.google.android.projection.gearhead"
"com.google.android.youtube"
"com.google.ar.core"
"com.google.mainline.adservices"
"com.google.mainline.telemetry"
"com.heytap.accessory"
"com.heytap.browser"
"com.heytap.market"
"com.microsoft.appmanager"
"com.microsoft.deviceintegrationservice"
"com.oplus.customize.coreapp"
"com.oplus.multiapp"
"com.oplus.themestore"
"com.oppo.quicksearchbox"
"com.taboola.scoop"
)

for app in "${list[@]}"
do
    echo "Disable $app"
    adb shell pm disable-user --user 0 "$app"

    # echo "Enable $app"
    # adb shell pm enable "$app"

    sleep 2
done
