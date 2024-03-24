#!/bin/bash

set -euo pipefail;

PATH="/home/linuxbrew/.linuxbrew/bin:$PATH"

rm -f ~/.config/sway/locksreen.png;
grim ~/.config/sway/lockscreen.png;
convert -filter Gaussian -resize 20% -blur 0x2.5 -resize 500% ~/.config/sway/lockscreen.png ~/.config/sway/lockscreen.png;
swaylock -e -f -i ~/.config/sway/lockscreen.png;

