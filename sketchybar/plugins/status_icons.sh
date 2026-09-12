#!/bin/bash

# Shared status badge map. Used for both ghostty and claude status

STATUS_ICON_WORKING=""     # nf-fa-spinner: busy with something
STATUS_ICON_WAITING=""     # nf-fa-bell: it is asking you something
STATUS_ICON_DONE=""        # nf-fa-check: the turn is finished
STATUS_ICON_SHELL=""       # nf-fa-terminal: dropped into a shell

STATUS_COLOR_WORKING=0xffe0af68  # yellow
STATUS_COLOR_WAITING=0xfff7768e  # red
STATUS_COLOR_DONE=0xff9ece6a     # green
STATUS_COLOR_SHELL=0xff7aa2f7    # blue

# Echoes "<icon>\t<color>" for a status, and nothing for an empty or unknown
# one (a window with nothing to report draws no badge). $1 = status.
get_status_badge() {
  case "$1" in
    working) printf '%s\t%s\n' "$STATUS_ICON_WORKING" "$STATUS_COLOR_WORKING" ;;
    waiting) printf '%s\t%s\n' "$STATUS_ICON_WAITING" "$STATUS_COLOR_WAITING" ;;
    "done")  printf '%s\t%s\n' "$STATUS_ICON_DONE"    "$STATUS_COLOR_DONE" ;;
    shell)   printf '%s\t%s\n' "$STATUS_ICON_SHELL"   "$STATUS_COLOR_SHELL" ;;
    *)       return 0 ;;
  esac
}
