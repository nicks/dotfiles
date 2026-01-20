#!/usr/bin/env bash

avail=$(df -lh | awk '$9 == "/" { print $4 }')
sketchybar -m --set disk_percentage label=$avail
