#!/bin/bash

rclone sync <path/sync_folder> gdrive:sync_folder

retVal=$?
if [ $retVal -ne 0 ]; then
    notify-send --urgency=critical 'Rclone service' "Failed with service code $retVal"
fi
