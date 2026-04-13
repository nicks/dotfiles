#!/bin/bash

focused_monitor=$(aerospace list-monitors --focused | awk '{print $1}')
focused=$(aerospace list-workspaces --focused | tr -d '\n')
for sid in $(aerospace list-workspaces --all); do
    count=$(aerospace list-windows --workspace $sid | wc -l | tr -d ' ')
    if [[ "$focused" != "$sid" && "$count" == "0" ]]; then
        aerospace workspace "$sid"
        aerospace move-workspace-to-monitor "$focused_monitor"
        exit 0
    fi
done
