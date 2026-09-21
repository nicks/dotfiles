#!/bin/bash

set -exuo pipefail

cd "$(dirname $0)"

if [[ "$(uname -s)" == "Linux" ]]; then
  cargo install sway-new-workspace
fi

python3 -m pip install webdiff --break-system-packages

# holo-layer (the emacs cursor tail) draws its overlay from a PyQt process that
# emacs talks to over epc.
HOLO_LAYER_VENV="$HOME/.local/venvs/holo-layer"
if [[ ! -x "$HOLO_LAYER_VENV/bin/python" ]]; then
  python3 -m venv "$HOLO_LAYER_VENV"
fi
"$HOLO_LAYER_VENV/bin/python" -m pip install --upgrade pip
"$HOLO_LAYER_VENV/bin/python" -m pip install -r python/holo-layer-requirements.txt

for dir in environment.d sway ghostty; do
  if [[ ! -d ~/.config/$dir ]]; then
    ln -s "$(pwd)/$dir" "$HOME/.config/$dir"
  fi
done

if [[ ! -f ~/.config/starship.toml ]]; then
  ln -s "$(pwd)/starship.toml" "$HOME/.config/starship.toml"
fi

mkdir -p ~/.config/jj
if [[ ! -L ~/.config/jj/config.toml ]]; then
  ln -sf "$(pwd)/jj/config.toml" "$HOME/.config/jj/config.toml"
fi

mkdir -p ~/.config/rift
if [[ ! -L ~/.config/rift/config.toml ]]; then
  ln -sf "$(pwd)/rift/config.toml" "$HOME/.config/rift/config.toml"
fi

for rc in .profile .bash_profile .bashrc .zprofile .zshrc; do
  if [[ ! -f ~/$rc ]]; then
    ln -s "$(pwd)/$rc" "$HOME/$rc"
  fi
done

mkdir -p ~/.ssh
chmod 700 ~/.ssh
if [[ ! -L ~/.ssh/config ]]; then
  ln -sf "$(pwd)/ssh/config" "$HOME/.ssh/config"
fi

# One-time: store the key's passphrase in the login Keychain so ssh can use it
# unattended from then on.
if [[ "$(uname -s)" == "Darwin" && -f ~/.ssh/id_ed25519 ]]; then
  ssh-add --apple-use-keychain ~/.ssh/id_ed25519
fi

mkdir -p ~/.claude
if [[ ! -L ~/.claude/CLAUDE.md ]]; then
  ln -sf "$(pwd)/claude/CLAUDE.md" "$HOME/.claude/CLAUDE.md"
fi

if [[ ! -f ~/.emacs ]]; then
  ln -s "$(pwd)/.emacs" "$HOME/.emacs"
fi

if [[ ! -f ~/.Brewfile ]]; then
  ln -s "$(pwd)/.Brewfile" "$HOME/.Brewfile"
fi

