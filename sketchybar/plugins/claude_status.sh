#!/bin/bash

# Claude Code status for terminal windows, sourced by rift_item.sh so the window
# strip can show what each Claude session is doing.
#
# Claude Code sets the terminal title to "<mark> <session title>", where <mark>
# is an animated spinner frame while the model is working and a static asterisk
# when it isn't. That separates working from not-working, but not "waiting on an
# answer from you" from "finished" — both show the static mark. To tell those
# apart we join the window back to Claude's own per-session state file under
# ~/.claude/sessions/<pid>.json, which records an explicit status.
#
# The join key is the session's generated title: the state file gives the session
# id and cwd, and the session transcript records the title Claude puts in the
# terminal. Transcripts run to megabytes, so titles are cached; a session whose
# title isn't cached yet still gets working/not-working from the mark alone.

CLAUDE_DIR="$HOME/.claude"
CLAUDE_TITLE_CACHE=/tmp/sketchybar_claude_titles
CLAUDE_TITLE_TTL=30

# The marks Claude Code prefixes its terminal title with.
CLAUDE_BUSY_MARKS="◐ ◑"
CLAUDE_QUIET_MARK="✳"

# Badge per status. Nerd Font glyphs; the palette matches ghostty/alacritty.
CLAUDE_ICON_BUSY=""        # nf-fa-asterisk: the model is working
CLAUDE_ICON_WAITING=""     # nf-fa-bell: it is asking you something
CLAUDE_ICON_IDLE=""        # nf-fa-check: the turn is finished
CLAUDE_ICON_SHELL=""       # nf-fa-terminal: dropped into a shell

CLAUDE_COLOR_BUSY=0xffe0af68     # yellow
CLAUDE_COLOR_WAITING=0xfff7768e  # red
CLAUDE_COLOR_IDLE=0xff9ece6a     # green
CLAUDE_COLOR_SHELL=0xff7aa2f7    # blue

# Echoes the title Claude generated for a session, or nothing if it has not
# named the session yet (a brand new session still shows its cwd).
claude_session_title() {
  local sid=$1 cwd=$2
  local cache="$CLAUDE_TITLE_CACHE/$sid"
  if [[ -f "$cache" ]]; then
    local age=$(( $(date +%s) - $(stat -f %m "$cache") ))
    if [[ $age -lt $CLAUDE_TITLE_TTL ]]; then cat "$cache"; return; fi
  fi

  # Claude names project directories by replacing every non-alphanumeric
  # character with a dash, so /Users/x/src -> -Users-x-src.
  local slug transcript title=""
  slug=$(printf '%s' "$cwd" | sed 's/[^A-Za-z0-9]/-/g')
  transcript="$CLAUDE_DIR/projects/$slug/$sid.jsonl"
  if [[ -f "$transcript" ]]; then
    title=$(grep -F '"type":"ai-title"' "$transcript" | tail -1 |
              "$JQ" -r '.aiTitle // empty' 2>/dev/null)
  fi

  mkdir -p "$CLAUDE_TITLE_CACHE"
  printf '%s' "$title" > "$cache"
  printf '%s' "$title"
}

# Echoes one "<status>\t<title>" line per live Claude Code session. bash 3.2 has
# no associative arrays, so callers look this table up by scanning it.
claude_session_table() {
  local f pid sid cwd status title
  for f in "$CLAUDE_DIR"/sessions/*.json; do
    [[ -f "$f" ]] || continue
    IFS=$'\t' read -r pid sid cwd status < <(
      "$JQ" -r '[(.pid|tostring), .sessionId, .cwd, (.status // "")] | @tsv' "$f" 2>/dev/null)
    # Claude does not always clean up after a crash; skip sessions that are gone.
    [[ -n "$pid" ]] && kill -0 "$pid" 2>/dev/null || continue
    title=$(claude_session_title "$sid" "$cwd")
    [[ -n "$title" ]] && printf '%s\t%s\n' "$status" "$title"
  done
}

# Echoes "<icon>\t<color>" when a window title says it is running Claude Code,
# and nothing otherwise. $1 = window title, $2 = table from claude_session_table.
claude_badge() {
  local title=$1 table=$2 mark rest status
  mark=${title%% *}
  rest=${title#* }

  case " $CLAUDE_BUSY_MARKS " in
    *" $mark "*)
      status=busy ;;
    *)
      [[ "$mark" == "$CLAUDE_QUIET_MARK" ]] || return 0
      # The mark only says the model stopped; the session file says why.
      status=$(printf '%s\n' "$table" | awk -F'\t' -v t="$rest" '$2 == t { print $1; exit }')
      case "$status" in waiting|shell) ;; *) status=idle ;; esac ;;
  esac

  case "$status" in
    busy)    printf '%s\t%s\n' "$CLAUDE_ICON_BUSY" "$CLAUDE_COLOR_BUSY" ;;
    waiting) printf '%s\t%s\n' "$CLAUDE_ICON_WAITING" "$CLAUDE_COLOR_WAITING" ;;
    shell)   printf '%s\t%s\n' "$CLAUDE_ICON_SHELL" "$CLAUDE_COLOR_SHELL" ;;
    *)       printf '%s\t%s\n' "$CLAUDE_ICON_IDLE" "$CLAUDE_COLOR_IDLE" ;;
  esac
}
