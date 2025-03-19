#!/bin/bash

network-check.sh
retVal=$?
if [ $retVal -ne 0 ]; then
  notify-send --urgency=critical 'Drive service' "Network failed with service code $retVal"
  exit 1
fi

cd /data/backup/drive

rclone bisync drive: /data/drive --remove-empty-dirs --verbose >> /tmp/drive.log

retVal=$?
if [ $retVal -ne 0 ]; then
  notify-send --urgency=critical 'Drive service' "Failed with service code $retVal"
  exit 1
fi

rclone sync /data/drive mega:sync --verbose >> /tmp/drive.log

retMegaVal=$?
if [ $retMegaVal -ne 0 ]; then
  notify-send --urgency=critical 'Mega service' "Failed with service code $retVal"
  exit 1
fi
