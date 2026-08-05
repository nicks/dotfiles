#!/bin/bash

# Focus the display in the given direction by raising a window on it.
#
#   rift-focus-display.sh <up|down|left|right>
#
# i haven't been able to get rift's built-in focus_display to work,
# this is a hack until i have more time to look into it.
set -euo pipefail

dir="${1:?usage: $(basename "$0") <up|down|left|right>}"

displays=$(rift-cli query displays)

# Current display = the one holding the active focus context (reliable signal).
cur=$(printf '%s' "$displays" | jq -c '[.[] | select(.is_active_context)][0]')
[ "$cur" = "null" ] && exit 0
cx=$(printf '%s' "$cur" | jq '.frame.origin.x + .frame.size.width  / 2')
cy=$(printf '%s' "$cur" | jq '.frame.origin.y + .frame.size.height / 2')

# Nearest display strictly in the requested direction (squared distance avoids
# needing abs); empty if there is none that way.
target=$(printf '%s' "$displays" | jq -c --argjson cx "$cx" --argjson cy "$cy" --arg dir "$dir" '
  [ .[]
    | (.frame.origin.x + .frame.size.width  / 2) as $x
    | (.frame.origin.y + .frame.size.height / 2) as $y
    | select(
        ($dir == "left"  and $x < $cx) or
        ($dir == "right" and $x > $cx) or
        ($dir == "up"    and $y < $cy) or
        ($dir == "down"  and $y > $cy)
      )
    | { space: .space,
        dist: (if $dir == "left" or $dir == "right"
               then ($x - $cx) * ($x - $cx)
               else ($y - $cy) * ($y - $cy) end) }
  ] | sort_by(.dist) | .[0].space // empty')

[ -z "$target" ] && exit 0

# Raise the last-focused window on that display, else its first window.
win=$(rift-cli query workspaces --space-id "$target" \
  | jq -c '[.[] | select(.is_active) | .windows[]] as $w
           | (($w | map(select(.is_focused)))[0] // $w[0]).id // empty')
[ -z "$win" ] && exit 0

rift-cli execute window focus --window-id "$win"
