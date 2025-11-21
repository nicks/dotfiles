#!/bin/bash

focused=$(aerospace list-workspaces --focused | tr -d '\n')
for sid in $(aerospace list-workspaces --all); do
    count=$(aerospace list-windows --workspace $sid | wc -l | tr -d ' ')
    if [[ "$focused" != "$sid" && "$count" == "0" ]]; then
        aerospace move-node-to-workspace --focus-follows-window "$sid"
    fi
done
