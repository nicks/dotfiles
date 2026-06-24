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


focused_window=$(aerospace list-windows --focused | cut -d' ' -f1 | tr -d ' ')

shift_workspace_right_ignoring_focused() {
  local from="$1"
  local to="$(( from + 1 ))"

  # base case: there are no windows to move in 'from'
  local from_windows=$(aerospace list-windows --workspace "$from" | cut -d' ' -f1 | grep -v "$focused_window")
  if [[ -z "$from_windows" ]]; then
    return
  fi

  # shift the 'to' workspace out of the way
  shift_workspace_right_ignoring_focused "$to"

  # move all the windows
  for win in $from_windows; do
    aerospace move-node-to-workspace --window-id "$win" "$to"
  done
}

# It's easier to think of this is 3 actions:
# focus: find the next non-empty workspace, focus it
# move (empty workspace): find the next non-empty workspace, move the focused window there
# move (non-empty workspace): shift all the windows in this workspace out of the way
if [[ "$action" == "move" ]]; then
  focused_workspace=$(aerospace list-workspaces --focused | tr -d '\n')
  count=$(aerospace list-windows --workspace "$focused_workspace" | wc -l | tr -d ' ')
  if [[ "$count" != "1" ]]; then
    
    # handle the shift case
    if [[ "$mode" == "prev" ]]; then
      # Move all windows in the current workspace to the next workspace, so that
      # the current window is alone.
      shift_workspace_right_ignoring_focused "$focused_workspace"
    else
      # Move all windows in the next workspace to make way for this window.
      shift_workspace_right_ignoring_focused "$(( focused_workspace + 1 ))"
      aerospace move-node-to-workspace --focus-follows-window "$(( focused_workspace + 1 ))"
    fi
    exit 0
    
  fi
fi

# focus and move actions: search for the workspace we want
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

  # empty workspaces should only be considered if they're the currenly
  # focused workspace
  if [[ "$count" == "0" ]] && [[ "$focused" != "$sid" ]]; then
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

