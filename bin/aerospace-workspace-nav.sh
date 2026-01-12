#!/bin/bash

# Takes two arguments:
# aerospace-workspace-helper.sh <prev|next> <focus|move>
mode="$1"
action="$2"
if [[ "$mode" != "prev" && "$mode" != "next" ]]; then
  echo "Invalid mode: $mode. Use 'prev' or 'next'."
  exit 1
fi
if [[ "$action" != "focus" && "$action" != "move" ]]; then
  echo "Invalid action: $action. Use 'focus' or 'move'."
  exit 1
fi

focused=$(aerospace list-workspaces --focused | tr -d '\n')
first="0"
next="0"
if [[ "$mode" == "prev" ]]; then
  workspaces=$(aerospace list-workspaces --all | tac)
else
  workspaces=$(aerospace list-workspaces --all)
fi
for sid in $(echo "$workspaces"); do
  count=$(aerospace list-windows --workspace $sid | wc -l | tr -d ' ')
  if [[ "$count" == "0" ]]; then
    continue
  fi
  if [[ "$first" == "0" ]]; then
    first="$sid"
  fi
  if [[ "$next" == "true" ]]; then
    if [[ "$action" == "focus" ]]; then
      aerospace workspace "$sid"
    else
      aerospace move-node-to-workspace --focus-follows-window "$sid"
    fi
    exit 0
  fi
  if [[ "$focused" == "$sid" ]]; then
    next="true"
  fi
done

if [[ "$action" == "focus" ]]; then
  aerospace workspace "$first"
else
  aerospace move-node-to-workspace --focus-follows-window "$first"
fi

