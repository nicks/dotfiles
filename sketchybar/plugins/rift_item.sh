#!/bin/bash

# Renders one sketchybar item per window in each monitor's active workspace, in
# rift's on-screen strip order: displays left-to-right, and within each display
# the layout order rift reports. Each window is drawn only on its own display's
# bar, so a monitor's strip shows just that monitor's windows.

source "$CONFIG_DIR/plugins/app_icons.sh"

# rift-cli lives under Homebrew, which differs by arch (/opt/homebrew on Apple
# Silicon, /usr/local on Intel). sketchybar launches this plugin with a minimal
# PATH, so command -v may miss it — fall back to the known Homebrew locations.
RIFT_CLI=$(command -v rift-cli)
[[ -x "$RIFT_CLI" ]] || for p in /opt/homebrew/bin/rift-cli /usr/local/bin/rift-cli; do
  [[ -x "$p" ]] && { RIFT_CLI=$p; break; }
done
JQ=$(command -v jq)

source "$CONFIG_DIR/plugins/status_icons.sh"
source "$CONFIG_DIR/plugins/claude_status.sh"
source "$CONFIG_DIR/plugins/terminal_status.sh"

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
# rift returns an error payload as a JSON object (not an array) with exit 0, so
# every query result is type-checked before use.
is_array() { [[ "$(echo "$1" | "$JQ" -r 'if type == "array" then "y" else "n" end' 2>/dev/null)" == "y" ]]; }

displays=$("$RIFT_CLI" query displays 2>/dev/null) || exit 0
is_array "$displays" || exit 0

# rift and sketchybar key displays by the same UUID, so a rift display maps onto
# sketchybar's arrangement id (1-based) and its window items get pinned to that
# display's bar. A display sketchybar doesn't know about falls back to "all".
sb_displays=$(sketchybar --query displays 2>/dev/null)
display_index() {
  local idx
  idx=$(echo "$sb_displays" | "$JQ" -r --arg u "$1" '.[] | select(.UUID == $u) | ."arrangement-id"' 2>/dev/null)
  echo "${idx:-all}"
}

order=()                       # window-server ids, in strip order
# Per-window attributes, all keyed by window-server id. macOS ships bash 3.2,
# which has no associative arrays — these stay plain indexed arrays, so every
# lookup key here has to be numeric.
# `query displays` lists displays left-to-right, and `query windows --display`
# returns that display's active workspace in layout order, so walking the two in
# sequence yields the on-screen strip order directly.
while IFS= read -r uuid; do
  [[ -z "$uuid" ]] && continue
  wins=$("$RIFT_CLI" query windows --display "$uuid" 2>/dev/null) || exit 0
  # A failed read mid-transition would wipe the bar; leave it alone instead.
  is_array "$wins" || exit 0

  disp=$(display_index "$uuid")
  while IFS=$'\t' read -r idx pid wsid app focused title; do
    [[ -z "$wsid" ]] && continue
    order+=("$wsid")
    W_IDX[$wsid]=$idx; W_PID[$wsid]=$pid; W_APP[$wsid]=$app; W_FOCUS[$wsid]=$focused
    W_TITLE[$wsid]=$title; W_DISPLAY[$wsid]=$disp
  done < <(echo "$wins" | "$JQ" -r '.[] | [(.id.idx|tostring),(.id.pid|tostring),(.window_server_id|tostring),.app_name,(.is_focused|tostring),.title] | @tsv')
done < <(echo "$displays" | "$JQ" -r '.[].uuid')

# If we gathered nothing but rift actually has windows, it was a transient read.
total=$("$RIFT_CLI" query windows 2>/dev/null | "$JQ" 'if type == "array" then length else 0 end' 2>/dev/null)
[[ ${#order[@]} -eq 0 && "${total:-0}" -gt 0 ]] && exit 0

# --- reconcile the bar to the desired set ---------------------------------
claude_table=$(claude_session_table)
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
  # Window status for claude and terminal windows
  status=$(claude_status "${W_TITLE[$wsid]}" "$claude_table")
  if [[ -n "$status" ]]; then
    app_icon=$CLAUDE_ICON
  elif term_reports_command "${W_APP[$wsid]}"; then
    status=$(term_status "${W_TITLE[$wsid]}")
  fi

  # draw the status badge on the top-right, slightly overlapping
  # the app icon
  badge=$(get_status_badge "$status")
  if [[ -n "$badge" ]]; then
    IFS=$'\t' read -r badge_icon badge_color <<< "$badge"
    badge_args=(label="$badge_icon" label.color=$badge_color label.drawing=on)
    icon_padding_right=-4
  else
    badge_args=(label.drawing=off)
    icon_padding_right=8
  fi

  # focus needs both the rift window id (JSON) and the window-server id.
  click="$RIFT_CLI execute window focus --window-id '{\"pid\":${W_PID[$wsid]},\"idx\":${W_IDX[$wsid]}}' --window-server-id $wsid"

  sketchybar --add item "$item" left 2>/dev/null
  sketchybar --set "$item" \
             icon.font="FiraCode Nerd Font:Regular:15.0" \
             icon="$app_icon" \
             icon.color=$color \
             icon.padding_left=8 \
             icon.padding_right=$icon_padding_right \
             "${badge_args[@]}" \
             label.font="FiraCode Nerd Font:Regular:12.0" \
             label.padding_left=0 \
             label.padding_right=8 \
             label.y_offset=9 \
             background.color=$PILL_COLOR \
             background.corner_radius=6 \
             background.height=26 \
             background.drawing=$draw \
             display=${W_DISPLAY[$wsid]} \
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
