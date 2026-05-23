#!/bin/bash

set -exuo pipefail

cd "$(dirname $0)"

if [[ "$(uname -s)" == "Linux" ]]; then
  cargo install sway-new-workspace
fi

python3 -m pip install webdiff --break-system-packages

for dir in environment.d sway ghostty; do
  if [[ ! -d ~/.config/$dir ]]; then
    ln -s "$(pwd)/$dir" "$HOME/.config/$dir"
  fi
done

if [[ ! -f ~/.config/starship.toml ]]; then
  ln -s "$(pwd)/starship.toml" "$HOME/.config/starship.toml"
fi

mkdir -p ~/.config/hunk
if [[ ! -f ~/.config/hunk/config.toml ]]; then
  ln -s "$(pwd)/hunk/config.toml" "$HOME/.config/hunk/config.toml"
fi

mkdir -p ~/.config/jj
if [[ ! -L ~/.config/jj/config.toml ]]; then
  ln -sf "$(pwd)/jj/config.toml" "$HOME/.config/jj/config.toml"
fi

if [[ ! -f ~/.zshrc ]]; then
  ln -s "$(pwd)/.zshrc" "$HOME/.zshrc"
fi

if [[ ! -f ~/.emacs ]]; then
  ln -s "$(pwd)/.emacs" "$HOME/.emacs"
fi

if [[ ! -f ~/.Brewfile ]]; then
  ln -s "$(pwd)/.Brewfile" "$HOME/.Brewfile"
fi

