#!/bin/bash

cd /media/data/work/backup/drive/
grive -s keepass/
retVal=$?
if [ $retVal -ne 0 ]; then
    notify-send --urgency=critical 'Grive service' "Failed with service code $retVal"
fi
