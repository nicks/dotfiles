#!/bin/bash

# Running-command status for terminal windows, sourced by rift_item.sh as a
# fallback for windows claude_status.sh does not claim. Statuses are the generic
# ones from status_icons.sh, so a shell running a build wears the same badge as
# a Claude session that is thinking.
#
# No process inspection is involved: Ghostty's shell integration already sets
# the window title to the command it is running, and back to the working
# directory at the prompt. So a title that does not look like a path means that
# window is busy with something.
#
#   ~/src/tilt    -> at a prompt
#   …/src/long-repo-name/mobile-client -> at a prompt, path abbreviated by ghostty
#   brew upgrade  -> running a command
#
# Terminals whose titles do not work this way are left alone below, or they
# would wear the badge permanently.

# Apps whose shell integration puts the running command in the window title.
# Alacritty and Terminal.app do not, so they stay off this list.
TERM_TITLE_APPS="Ghostty"

# True when $1 is an app whose title tracks the running command.
term_reports_command() {
  case " $TERM_TITLE_APPS " in
    *" $1 "*) return 0 ;;
    *)        return 1 ;;
  esac
}

# Echoes "working" when a window title says a command is running, and nothing
# otherwise. $1 = window title.
term_status() {
  case "$1" in
    # A path (or nothing yet) is Ghostty reporting the cwd at an idle prompt.
    # Ghostty abbreviates a cwd four or more components deep to "…/last/three".
    ""|/*|"~"|"~/"*|"…/"*) return 0 ;;
    *) printf 'working\n' ;;
  esac
}
