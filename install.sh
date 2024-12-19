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
