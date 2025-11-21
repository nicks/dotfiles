#!/bin/bash

focused=$(aerospace list-workspaces --focused | tr -d '\n')
previous="aerospace"
for sid in $(aerospace list-workspaces --all); do
    count=$(aerospace list-windows --workspace $sid | wc -l | tr -d ' ')
    if [[ "$focused" != "$sid" && "$count" == "0" ]]; then
        aerospace workspace "$sid"
        exit 0
    fi
done
