#!/bin/bash

rclone sync --no-update-modtime <path/sync_folder> gdrive:sync_folder

retVal=$?
if [ $retVal -ne 0 ]; then
    notify-send --urgency=critical 'Rclone service' "Failed with service code $retVal"
fi
