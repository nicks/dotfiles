DOTFILES="$HOME/src/dotfiles"

# Re-source env in case this is a non-login shell (common on Linux).
. "$DOTFILES/shell/env.sh"
. "$DOTFILES/shell/aliases.sh"
. "$DOTFILES/shell/interactive.sh"

# Zsh-specific.
eval "$(starship init zsh)"

if command -v brew >/dev/null; then
  fpath=($(brew --prefix)/share/zsh-completions $(brew --prefix)/share/zsh/site-functions $fpath)
fi
fpath=($HOME/.docker/completions $fpath)

autoload -Uz compinit
compinit -u

command -v jj >/dev/null && source <(jj util completion zsh)
