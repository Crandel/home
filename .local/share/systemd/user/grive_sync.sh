#!/bin/bash

cd <path>
grive -s <folder>/
retVal=$?
if [ $retVal -ne 0 ]; then
    notify-send --urgency=critical 'Grive service' "Failed with service code $retVal"
else
    notify-send --urgency=normal 'Grive service' "Sync was successful"
fi
