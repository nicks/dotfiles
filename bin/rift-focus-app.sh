#!/bin/bash

# Focus an app's first window, launching the app if it isn't running.
#
#   rift-focus-app.sh <app-name>
#
# "First" means the lowest rift window index, i.e. the oldest window the app has
# open. Scrolling columns get reordered constantly, so keying off position would
# make the shortcut land somewhere different every time; window age is stable.
set -euo pipefail

app="${1:?usage: $(basename "$0") <app-name>}"

win=$(rift-cli query windows \
  | jq -c --arg app "$app" '[.[] | select(.app_name == $app)]
                            | sort_by(.id.idx) | .[0].id // empty')

if [ -z "$win" ]; then
  open -a "$app"
  exit 0
fi

rift-cli execute window focus --window-id "$win"
