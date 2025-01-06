#!/bin/bash

set -euo pipefail

# exit if number of arguments is 1
if [[ "$#" -ne 1 ]]; then
    echo "invalid number of arguments"
    exit
fi

MODE="$1"

swaymsg -t get_outputs -r | jq -r '.[] | .name' | while read -r output; do
    swaymsg "output $output power $MODE"
done

