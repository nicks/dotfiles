# Shared environment — POSIX-compatible.
# Sourced by .profile, .zprofile, and (idempotently) by .bashrc/.zshrc
# so Linux terminal emulators that start non-login shells still get PATH.

export EDITOR="emacsclient -t -a ''"
export SKIP=pytest

# Directories to prepend to PATH. Each is added only if it exists on disk
# and isn't already on PATH (so re-sourcing this file is a no-op).
# Order: entries lower in the list end up earlier on PATH (higher priority).
PATH_DIRS="
/opt/homebrew/opt/node@24/bin
/home/linuxbrew/.linuxbrew/bin
$HOME/src/dotfiles/bin
$HOME/.local/bin
$HOME/bin
$HOME/go/bin
$HOME/.cargo/bin
"

for dir in $PATH_DIRS; do
  [ -d "$dir" ] || continue
  case ":$PATH:" in
    *":$dir:"*) ;;
    *) PATH="$dir:$PATH" ;;
  esac
done
unset PATH_DIRS dir
export PATH
