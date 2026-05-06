#!/bin/bash

set -exuo pipefail

cd "$(dirname $0)"

cargo install sway-new-workspace
python3 -m pip install webdiff --break-system-packages

for dir in environment.d sway; do
  if [[ ! -d ~/.config/$dir ]]; then
    ln -s "$(pwd)/$dir" "$HOME/.config/$dir"
  fi
done

if [[ ! -f ~/.config/starship.toml ]]; then
  ln -s "$(pwd)/starship.toml" "$HOME/.config/starship.toml"
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

if [[ ! -f ~/Brewfile ]]; then
    ln -s "$(pwd)/.Brewfile" "$HOME/.Brewfile"
fi

