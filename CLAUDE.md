# Color palette locations

When changing a shared color, update all of these so they stay in sync.

## Shared palette (Tokyo Night-flavored, with a custom bright green)

Terminal/editor color palette — the canonical 16-color set lives in three places that should match:

- `ghostty/config` — `background`, `foreground`, and `palette = N=#RRGGBB` (entries 0–15)
- `alacritty/alacritty.toml` — `[colors.primary]`, `[colors.normal]`, `[colors.bright]`
- `.emacs` — `custom-set-faces` block: `rainbow-delimiters-depth-*-face` and `font-lock-*-face`, both mapped to the ghostty palette. Plus `set-background-color`, `set-foreground-color`, the `mode-line` face, and `ansi-color-names-vector` for compile/shell buffers

### Master palette — every color used across emacs/ghostty/alacritty/sketchybar

ANSI 0–7 (ghostty `palette = N`, alacritty `[colors.normal]`, emacs `ansi-color-names-vector`):

| ANSI | hex | role |
|---|---|---|
| 0 black   | `#000000` | terminal black (same as the background) |
| 1 red     | `#f7768e` | emacs `font-lock-builtin/warning/negation`; sketchybar Claude badge "waiting on you" |
| 2 green   | `#9ece6a` | rainbow-delimiters depth 4/8; emacs `font-lock-string-face`; sketchybar Claude badge "done" |
| 3 yellow  | `#e0af68` | emacs `font-lock-variable-name-face`; rainbow-delimiters depth 3/7; sketchybar Claude badge "working" |
| 4 blue    | `#7aa2f7` | emacs mode-line fg, `font-lock-function-name-face`; rainbow-delimiters depth 6; sketchybar Claude badge "in a shell" |
| 5 magenta | `#bb9af7` | emacs `font-lock-keyword/constant/preprocessor`; rainbow-delimiters depth 5/9 |
| 6 cyan    | `#7dcfff` | sketchybar monitor 2; emacs `font-lock-type-face`; rainbow-delimiters depth 2 |
| 7 white   | `#ffffff` | foreground; rainbow-delimiters depth 1 |

ANSI 8–15 (`[colors.bright]`) repeat 1–7 except palette 8 = `#414868` (bright black, currently unused outside the palette itself).

Other colors deliberately used:
- `#000000` — global background (ghostty, alacritty, emacs); also ANSI black
- `#15161e` — emacs mode-line background (`my/mode-line-background`)
- `#66ff66` — bright green, sketchybar monitor 1 (more vivid than the palette green)
- `#737aa2` — gray, emacs comment/doc faces (brighter than palette 8)
- `#ff9933` — orange, sketchybar monitor 3 (no orange in the ghostty palette)

Sketchybar-only utility colors (`sketchybar/sketchybarrc`, `sketchybar/plugins/power.sh`):
- `0xff323232` — bar background
- `0xff3b4252` — stats item background (Nord-ish dark gray)
- `0xff8b0a0d` — dark red app-icon highlight
- `0xff10528c` — dark blue app-icon highlight
- `0xffff3333` — low-battery red
- `0x44ffffff` — translucent white workspace pill bg


## Sketchybar

- `sketchybar/plugins/claude_status.sh` — Claude Code status badge colors (working/waiting/done/shell), drawn on the window items by `rift_item.sh`
- `sketchybar/plugins/aerospace_item.sh` — per-monitor workspace label colors (green/blue/orange for monitors 1/2/3). Sketchybar uses `0xAARRGGBB`, so `#66ff66` becomes `0xff66ff66`.
- `sketchybar/sketchybarrc` — bar background, default icon/label colors, app icon highlight colors, stats background
- `sketchybar/plugins/power.sh` — battery color thresholds

## Linux desktop (sway/i3)

- `mako/config` — notification background/border
- `waybar/style.css` — bar and module styling
