# Colors

Tokyo Night-flavored 16-color palette, with a custom bright green for cursors.

## Where the palette lives

`ghostty/config` is canonical: `background`, `foreground`, and `palette = N=#RRGGBB`
for ANSI 0–15. Two files mirror it and must be updated in the same change:

- `alacritty/alacritty.toml` — `[colors.primary]`, `[colors.normal]` (0–7),
  `[colors.bright]` (8–15)
- `.emacs` — `set-background-color`, `set-foreground-color`,
  `ansi-color-names-vector`, and the `custom-set-faces` block, where every
  `font-lock-*-face` and `rainbow-delimiters-depth-*-face` is deliberately mapped
  onto an ANSI slot rather than given its own color

Bright black (palette 8, `#414868`) is the one slot unused outside the palette
definitions themselves.

## Off-palette colors

A few colors are intentionally *not* from the 16: the bright green cursor
(`#66ff66`, shared by ghostty's `cursor-color` — which `cursor_tail.glsl`
follows — and emacs' `my/palette-bright-green` for both the frame cursor and
`holo-layer-cursor-color`), the emacs mode-line background and comment gray, and
sketchybar's bar/badge chrome. Grep for the literal hex before assuming a color
is used in one place only.

## Sketchybar

Sketchybar takes `0xAARRGGBB`, so `#66ff66` is written `0xff66ff66`. Colors are
set in:

- `sketchybar/plugins/status_icons.sh` — the one palette for window status
  badges (working / waiting / done / shell). `claude_status.sh` and
  `terminal_status.sh` only classify; they both render through this file.
- `sketchybar/plugins/aerospace_item.sh` — workspace label color per monitor
- `sketchybar/plugins/power.sh` — battery color thresholds
- `sketchybar/sketchybarrc` — bar background, default icon/label colors, app
  icon highlights, stats background

## Linux desktop (sway/i3)

Independent of the above: `mako/config` (notifications) and `waybar/style.css`.
