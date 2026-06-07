eval "$(starship init zsh)"

fpath=($(brew --prefix)/share/zsh-completions $(brew --prefix)/share/zsh/site-functions $HOME/.docker/completions $fpath)
autoload -Uz compinit
compinit -u
source <(jj util completion zsh)
alias k="kubectl"
alias d="docker"
emacsclient -a false -e 't' >/dev/null 2>&1 || emacs --daemon
alias e="emacsclient -t -a ''"
export EDITOR="emacsclient -t -a ''"
export PATH=$HOME/go/bin:/opt/homebrew/opt/node@24/bin:$HOME/src/dotfiles/bin:$PATH
export SKIP=pytest
