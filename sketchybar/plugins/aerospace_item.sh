#!/bin/bash


focused=$(aerospace list-workspaces --focused | tr -d '\n')
previous="aerospace"
for sid in $(aerospace list-workspaces --all); do
    count=$(aerospace list-windows --workspace $sid | wc -l | tr -d ' ')
    if [[ "$focused" == "$sid" || "$count" != "0" ]]; then
        color=0xffffffff
        if [[ "$focused" == "$sid" ]]; then
            color=0xff66ff66
        fi
        sketchybar --add item space.$sid left \
                   --set space.$sid \
                   background.color=0x44ffffff \
                   background.corner_radius=5 \
                   background.height=20 \
                   background.drawing=off \
                   label="$sid" \
                   label.color=$color \
                   click_script="aerospace workspace $sid" \
                   script="$CONFIG_DIR/plugins/aerospace.sh $sid"
        sketchybar --move space.$sid after "$previous"
        previous="space.$sid"
    else
        sketchybar --remove space.$sid
    fi
done
