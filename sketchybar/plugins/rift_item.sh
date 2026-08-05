#!/bin/bash

# Renders one sketchybar item per window in each monitor's active workspace, in
# rift's on-screen strip order (left-to-right by column, top-to-bottom within a
# column). The focused window is drawn green with a selected pill; every other
# window is plain white. Clicking an item focuses that window.
#
# Robustness: sketchybar can fire several rift events at once, so runs are
# serialized with a lock, and a run that gets an empty/partial query result
# bails out instead of mutating the bar (which would otherwise leave it in a
# corrupted partial state that doesn't self-heal until the next event).

source "$CONFIG_DIR/plugins/app_icons.sh"

# rift-cli lives under Homebrew, which differs by arch (/opt/homebrew on Apple
# Silicon, /usr/local on Intel). sketchybar launches this plugin with a minimal
# PATH, so command -v may miss it — fall back to the known Homebrew locations.
RIFT_CLI=$(command -v rift-cli)
[[ -x "$RIFT_CLI" ]] || for p in /opt/homebrew/bin/rift-cli /usr/local/bin/rift-cli; do
  [[ -x "$p" ]] && { RIFT_CLI=$p; break; }
done
JQ=$(command -v jq)

FOCUS_COLOR=0xff66ff66   # bright green: the focused window
IDLE_COLOR=0xffffffff    # white: every other window
PILL_COLOR=0x44ffffff

# --- serialize concurrent invocations -------------------------------------
LOCK=/tmp/rift_sketchybar.lock
acquire() {
  if ( set -o noclobber; echo "$$" > "$LOCK" ) 2>/dev/null; then return 0; fi
  # Lock exists; steal it if the holder is no longer alive.
  local holder; holder=$(cat "$LOCK" 2>/dev/null)
  if [[ -n "$holder" ]] && ! kill -0 "$holder" 2>/dev/null; then
    rm -f "$LOCK"
    ( set -o noclobber; echo "$$" > "$LOCK" ) 2>/dev/null && return 0
  fi
  return 1
}
got=0
for _ in $(seq 1 40); do acquire && { got=1; break; }; sleep 0.05; done
# Another run holds the lock and will render the latest state; let it.
[[ $got -eq 1 ]] || exit 0
trap 'rm -f "$LOCK"' EXIT

# --- gather the desired window set ----------------------------------------
displays=$("$RIFT_CLI" query displays 2>/dev/null) || exit 0
[[ -z "$displays" ]] && exit 0

order=()                       # window-server ids, in strip order
declare -A W_PID W_IDX W_APP W_FOCUS
while IFS= read -r space_id; do
  [[ -z "$space_id" ]] && continue
  ws=$("$RIFT_CLI" query workspaces --space-id "$space_id" 2>/dev/null) || exit 0
  [[ -z "$ws" ]] && exit 0
  # A display with no rift workspaces at all: nothing to draw for it.
  [[ "$(echo "$ws" | "$JQ" 'length')" == "0" ]] && continue
  # We expect exactly one active workspace per display; if none is reported the
  # query caught a transition — bail rather than wipe the bar.
  active=$(echo "$ws" | "$JQ" -c 'map(select(.is_active))[0] // empty')
  [[ -z "$active" ]] && exit 0

  # Order windows left-to-right by column x, then top-to-bottom within a column.
  while IFS=$'\t' read -r idx pid wsid app focused; do
    [[ -z "$wsid" ]] && continue
    order+=("$wsid")
    W_IDX[$wsid]=$idx; W_PID[$wsid]=$pid; W_APP[$wsid]=$app; W_FOCUS[$wsid]=$focused
  done < <(echo "$active" | "$JQ" -r '.windows | sort_by(.frame.origin.x, .frame.origin.y) | .[] | [(.id.idx|tostring),(.id.pid|tostring),(.window_server_id|tostring),.app_name,(.is_focused|tostring)] | @tsv')
done < <(echo "$displays" | "$JQ" -r '.[].space')

# If we gathered nothing but rift actually has windows, it was a transient read.
total=$("$RIFT_CLI" query windows 2>/dev/null | "$JQ" 'length' 2>/dev/null)
[[ ${#order[@]} -eq 0 && "${total:-0}" -gt 0 ]] && exit 0

# --- reconcile the bar to the desired set ---------------------------------
declare -A want
previous="rift"
for wsid in "${order[@]}"; do
  want[$wsid]=1
  item="rift.win.$wsid"
  app_icon=$(get_app_icon "${W_APP[$wsid]}")
  if [[ "${W_FOCUS[$wsid]}" == "true" ]]; then
    color=$FOCUS_COLOR; draw=on
  else
    color=$IDLE_COLOR; draw=off
  fi
  # focus needs both the rift window id (JSON) and the window-server id.
  click="$RIFT_CLI execute window focus --window-id '{\"pid\":${W_PID[$wsid]},\"idx\":${W_IDX[$wsid]}}' --window-server-id $wsid"

  sketchybar --add item "$item" left 2>/dev/null
  sketchybar --set "$item" \
             icon.drawing=off \
             label.font="FiraCode Nerd Font:Regular:15.0" \
             label="$app_icon" \
             label.color=$color \
             label.padding_left=8 \
             label.padding_right=8 \
             background.color=$PILL_COLOR \
             background.corner_radius=6 \
             background.height=26 \
             background.drawing=$draw \
             click_script="$click"
  sketchybar --move "$item" after "$previous"
  previous="$item"
done

# Remove items for windows no longer shown, plus any leftover per-workspace
# items from the previous plugin version.
for item in $(sketchybar --query bar 2>/dev/null | "$JQ" -r '.items[] | select(startswith("rift.win.") or startswith("space."))'); do
  case "$item" in
    space.*)    sketchybar --remove "$item" ;;
    rift.win.*) wsid=${item#rift.win.}; [[ -z "${want[$wsid]}" ]] && sketchybar --remove "$item" ;;
  esac
done
