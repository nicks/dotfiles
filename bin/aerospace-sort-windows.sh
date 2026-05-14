#!/bin/bash
set -euo pipefail

focused_monitor=$(aerospace list-monitors --focused | awk '{print $1}')
focused_window=$(aerospace list-windows --focused --format '%{window-id}' 2>/dev/null || true)

windows_sorted=$(
    aerospace list-windows --monitor focused --format '%{window-id}|%{app-name}' \
    | awk -F'|' 'NF>=2 {key=tolower($2); printf "%s\t%s\n", key, $0}' \
    | sort \
    | cut -f2-
)

i=1
while IFS='|' read -r win_id app_name; do
    [[ -z "$win_id" ]] && continue
    aerospace move-node-to-workspace --window-id "$win_id" "$i"
    aerospace move-workspace-to-monitor --workspace "$i" "$focused_monitor" >/dev/null 2>&1 || true
    i=$((i+1))
done <<< "$windows_sorted"

if [[ -n "$focused_window" ]]; then
    aerospace focus --window-id "$focused_window" 2>/dev/null || true
fi
