# Shared environment — POSIX-compatible.
# Sourced by .profile, .zprofile, and (idempotently) by .bashrc/.zshrc
# so Linux terminal emulators that start non-login shells still get PATH.

export EDITOR="emacs -nw"
export SKIP=pytest

# Prepend a directory to PATH if it exists on disk.
#
# Strips any prior copy first, so re-sourcing is idempotent AND entries placed
# later by something else (e.g. macOS path_helper appending /opt/homebrew/bin)
# still get pulled to the front.
_prepend_path() {
  [ -d "$1" ] || return 0
  case ":$PATH:" in
    *":$1:"*) PATH=$(printf '%s' ":$PATH:" | sed -e "s|:$1:|:|g" -e 's|^:||' -e 's|:$||') ;;
  esac
  PATH="$1:$PATH"
}

# Order: later calls win — they end up earlier on PATH.
_prepend_path "/opt/homebrew/bin"
_prepend_path "/opt/homebrew/opt/node@24/bin"
_prepend_path "/home/linuxbrew/.linuxbrew/bin"
_prepend_path "/opt/homebrew/opt/postgresql@18/bin"
_prepend_path "$HOME/Library/pnpm"
# mise shims resolve each repo's pinned node, pnpm, and JDK in shells that
# never run `mise activate`, like editors and agent tool calls.
_prepend_path "$HOME/.local/share/mise/shims"
_prepend_path "$HOME/src/dotfiles/bin"
_prepend_path "$HOME/.local/bin"
_prepend_path "$HOME/bin"
_prepend_path "$HOME/go/bin"
_prepend_path "$HOME/.cargo/bin"

unset -f _prepend_path
export PATH
