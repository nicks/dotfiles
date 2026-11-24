#!/bin/bash

# Move focus one column left or right, wrapping onto the next display at the
# edge of the current one.
#
#   rift-focus-column.sh <left|right>
#
# Displays are ordered top-to-bottom, then left-to-right, and the order wraps
# around. Moving right off the last column lands on the next display's first
# column; moving left off the first column lands on the previous display's last
# column.
set -euo pipefail

dir="${1:?usage: $(basename "$0") <left|right>}"

displays=$(rift-cli query displays | jq -c 'sort_by(.frame.origin.y, .frame.origin.x)')
n=$(printf '%s' "$displays" | jq 'length')
cur=$(printf '%s' "$displays" | jq '[.[].is_active_context] | index(true) // empty')
[ -z "$cur" ] && exit 0

# Columns of the active workspace, each as a list of window ids.
columns() {
  rift-cli query layout --display "$1" \
    | jq -c '[.container_tree.children[]
              | [.. | objects | select(.node_type == "window") | .window_id]]'
}

uuid=$(printf '%s' "$displays" | jq -r --argjson i "$cur" '.[$i].uuid')
selected=$(rift-cli query layout --display "$uuid" | jq -c '.selected_window')
col=$(columns "$uuid" | jq --argjson w "$selected" 'map(index([$w]) != null) | index(true) // empty')
ncols=$(columns "$uuid" | jq 'length')

# Not on a tiled window, or not at the edge: ordinary column move.
if [ -z "$col" ] || { [ "$dir" = left ] && [ "$col" -gt 0 ]; } \
                 || { [ "$dir" = right ] && [ "$col" -lt $((ncols - 1)) ]; }; then
  exec rift-cli execute window focus "$dir"
fi

if [ "$dir" = right ]; then
  next=$(( (cur + 1) % n ))
else
  next=$(( (cur - 1 + n) % n ))
fi
target=$(printf '%s' "$displays" | jq -r --argjson i "$next" '.[$i].uuid')

# First window of the target display's near-edge column.
win=$(columns "$target" | jq -c --arg dir "$dir" \
  'if length == 0 then empty
   elif $dir == "right" then .[0][0] else .[-1][0] end')
[ -z "$win" ] && exit 0

rift-cli execute window focus --window-id "$win"
