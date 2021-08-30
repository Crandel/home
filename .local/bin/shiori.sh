#!/usr/bin/env bash

shiori ls -j | jq '.[] | .url' | bemenu -p bookmark -l 40 | firefox --new-tab
