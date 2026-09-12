#!/bin/bash

# Claude Code status for terminal windows, sourced by rift_item.sh so the window
# strip can show what each Claude session is doing. The statuses are the generic
# ones in status_icons.sh -- a thinking Claude and a running build wear the same
# badge; what marks a window as Claude Code is CLAUDE_ICON replacing its
# terminal app icon.
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

# The glyph a window running Claude Code wears in place of its terminal app
# icon, echoing the mark Claude puts in the title.
CLAUDE_ICON=""              # nf-fa-asterisk

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

# Echoes this window's status in the shared vocabulary (see status_icons.sh)
# when its title says it is running Claude Code, and nothing otherwise -- so a
# non-empty result also means "this is a Claude window".
# $1 = window title, $2 = table from claude_session_table.
claude_status() {
  local title=$1 table=$2 mark rest status
  mark=${title%% *}
  rest=${title#* }

  case " $CLAUDE_BUSY_MARKS " in
    *" $mark "*) printf 'working\n'; return 0 ;;
  esac
  [[ "$mark" == "$CLAUDE_QUIET_MARK" ]] || return 0

  # The mark only says the model stopped; the session file says why.
  status=$(printf '%s\n' "$table" | awk -F'\t' -v t="$rest" '$2 == t { print $1; exit }')
  case "$status" in
    waiting) printf 'waiting\n' ;;
    shell)   printf 'shell\n' ;;
    *)       printf 'done\n' ;;
  esac
}
