# Bail out for non-interactive shells (scp, rsync, etc.).
case $- in *i*) ;; *) return;; esac

DOTFILES="$HOME/src/dotfiles"

# Re-source env in case this is a non-login shell (common on Linux).
. "$DOTFILES/shell/env.sh"
. "$DOTFILES/shell/aliases.sh"
. "$DOTFILES/shell/interactive.sh"

# Bash-specific.
command -v starship >/dev/null && eval "$(starship init bash)"
command -v jj >/dev/null && eval "$(jj util completion bash)"
