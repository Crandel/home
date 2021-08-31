##!/usr/bin/env bash

/opt/Vieb/vieb --use-cmd-decoder=validating \
               --force_low_power_gpu \
               --ignore-gpu-blacklist \
               --enable-features=UseOzonePlatform,WebRTCPipeWireCapturer,WebContentsForceDark,VaapiVideoDecoder \
               --ozone-platform=wayland "$@"
