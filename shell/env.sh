# Shared environment — POSIX-compatible.
# Sourced by .profile, .zprofile, and (idempotently) by .bashrc/.zshrc
# so Linux terminal emulators that start non-login shells still get PATH.

export EDITOR="emacsclient -t -a ''"
export SKIP=pytest

# Prepend a directory to PATH if it exists on disk and isn't already there
# (so re-sourcing this file is a no-op). Order: later calls win — they end
# up earlier on PATH.
_prepend_path() {
  [ -d "$1" ] || return 0
  case ":$PATH:" in
    *":$1:"*) return 0 ;;
  esac
  PATH="$1:$PATH"
}

_prepend_path "/opt/homebrew/opt/node@24/bin"
_prepend_path "/home/linuxbrew/.linuxbrew/bin"
_prepend_path "$HOME/src/dotfiles/bin"
_prepend_path "$HOME/.local/bin"
_prepend_path "$HOME/bin"
_prepend_path "$HOME/go/bin"
_prepend_path "$HOME/.cargo/bin"

unset -f _prepend_path
export PATH
