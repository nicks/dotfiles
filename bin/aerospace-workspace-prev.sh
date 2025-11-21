#!/bin/bash

focused=$(aerospace list-workspaces --focused | tr -d '\n')
first="0"
next="0"
for sid in $(aerospace list-workspaces --all | tac); do
    count=$(aerospace list-windows --workspace $sid | wc -l | tr -d ' ')
    if [[ "$count" == "0" ]]; then
      continue
    fi
    if [[ "$first" == "0" ]]; then
      first="$sid"
    fi
    if [[ "$next" == "true" ]]; then
      aerospace workspace "$sid"
      exit 0
    fi
    if [[ "$focused" == "$sid" ]]; then
      next="true"
    fi
done

aerospace workspace "$first"
