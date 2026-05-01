eval "$(starship init zsh)"

fpath=($(brew --prefix)/share/zsh-completions $(brew --prefix)/share/zsh/site-functions $fpath)
autoload -Uz compinit
compinit -u
source <(jj util completion zsh)
alias k="kubectl"
alias d="docker"
alias e="emacs"
export EDITOR=emacs
export PATH=$HOME/src/dotfiles/bin:$PATH
export DYLD_FALLBACK_LIBRARY_PATH=/opt/homebrew/opt/libpq/lib
