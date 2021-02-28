#!/bin/bash

cd /data/backup/drive

grive -s <sync> \
      --id <id> \ 
      --secret <secret>

retVal=$?
if [ $retVal -ne 0 ]; then
  notify-send --urgency=critical 'Drive service' "Failed with service code $retVal"
  exit 1
fi
