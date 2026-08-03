#!/usr/bin/env bash
#
# Waybar custom module: an app glyph for each window on this bar's active niri
# workspace, in niri's column order, with the focused window highlighted.
# Floating windows come last. Glyphs come from shell/app_icons.sh.
#
# Prints one JSON line per change, so run it with "return-type": "json".

source "$(dirname "$0")/app_icons.sh"

render() {
  local windows icons
  windows=$(niri msg --json windows)
  # "app_id<TAB>glyph" per line, for every app with an open window
  icons=$(jq -r '.[].app_id // empty' <<<"$windows" | sort -u |
    while IFS= read -r app_id; do
      printf '%s\t%s\n' "$app_id" "$(get_app_icon "$app_id")"
    done)

  jq -cn \
    --arg output "${WAYBAR_OUTPUT_NAME:-}" \
    --arg icons "$icons" \
    --arg fallback "$(get_app_icon "")" \
    --argjson windows "$windows" \
    --argjson workspaces "$(niri msg --json workspaces)" '
    def esc: gsub("&"; "&amp;") | gsub("<"; "&lt;") | gsub(">"; "&gt;");

    ($icons | split("\n") | map(select(. != "") | split("\t") | {(.[0]): .[1]})
      | add // {}) as $icon_for
    | ($workspaces
      | map(select(if $output == "" then .is_focused
                   else .is_active and .output == $output end))
      | first | .id?) as $ws
    | [$windows[] | select($ws != null and .workspace_id == $ws)]
    | sort_by(if .is_floating then [1, .id]
              else [0] + .layout.pos_in_scrolling_layout end)
    | {
        text: map(($icon_for[.app_id // ""] // $fallback) as $icon
                  | if .is_focused
                    then "<span background=\"#285577\"> \($icon) </span>"
                    else " \($icon) " end)
              | join(" "),
        tooltip: map((if .is_focused then "▸ " else "  " end)
                     + (.title // "" | esc))
                 | join("\n"),
        class: (if length == 0 then "empty" else "" end)
      }'
}

render
niri msg --json event-stream | while read -r _; do
  render
done
