#!/bin/bash

cd /opt/work/backup/drive/

grive -s <sync> \
      --id <id> \ 
      --secret <secret>

retVal=$?
if [ $retVal -ne 0 ]; then
  notify-send --urgency=critical 'Grive service' "Failed with service code $retVal"
fi
