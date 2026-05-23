#!/bin/bash

# Find the lowest-numbered empty workspace and switch to it on the
# focused monitor. Searches beyond the currently-listed workspaces so
# that workspaces 10+ can be created on demand.

MAX_WORKSPACE=30

focused_monitor=$(aerospace list-monitors --focused | awk '{print $1}')
focused=$(aerospace list-workspaces --focused | tr -d '\n')

for ((sid=1; sid<=MAX_WORKSPACE; sid++)); do
    count=$(aerospace list-windows --workspace "$sid" 2>/dev/null | wc -l | tr -d ' ')
    if [[ "$focused" != "$sid" && "$count" == "0" ]]; then
        aerospace workspace "$sid"
        aerospace move-workspace-to-monitor "$focused_monitor"
        exit 0
    fi
done
