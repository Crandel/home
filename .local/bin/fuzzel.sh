#!/bin/sh

fuzzel --fuzzy-min-length=2 fuzzy-max-length-discrepancy=2 fuzzy-max-distance=1 $@
