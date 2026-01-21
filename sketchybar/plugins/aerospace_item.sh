#!/bin/bash

# Application icon mapping
get_app_icon() {
    local app_name="$1"
    case "$app_name" in
        "Google Chrome") echo "" ;;           # nf-md-chrome
        "Safari") echo "" ;;                  # nf-md-safari
        "Firefox") echo "" ;;                 # nf-md-firefox
        "Terminal") echo "" ;;               # nf-dev-terminal
        "iTerm2") echo "" ;;                  # nf-dev-terminal
        "Alacritty") echo "" ;;              # nf-dev-terminal
        "Emacs") echo "" ;;                   # nf-custom-emacs
        "Neovim") echo "" ;;                  # nf-dev-vim
        "Visual Studio Code") echo "" ;;     # nf-md-microsoft_visual_studio_code
        "Slack") echo "" ;;                   # nf-md-slack
        "Discord") echo "" ;;                 # nf-md-discord
        "Spotify") echo "" ;;                 # nf-md-spotify
        "Docker Desktop") echo "" ;;          # nf-linux-docker
        "Music") echo "" ;;                   # nf-md-music
        "Finder") echo "" ;;                  # nf-md-folder
        "System Settings") echo "" ;;         # nf-md-cog
        "Mail") echo "" ;;                    # nf-md-email
        "Calendar") echo "" ;;                # nf-md-calendar
        "Notes") echo "" ;;                   # nf-md-note
        "Photoshop") echo "" ;;               # nf-dev-photoshop
        "Figma") echo "" ;;                   # nf-md-figma
        "Zoom") echo "" ;;                    # nf-md-video
        "Telegram") echo "" ;;                # nf-md-telegram
        "WhatsApp") echo "" ;;               # nf-md-whatsapp
        "Preview") echo "" ;;                 # nf-md-image
        "TextEdit") echo "" ;;                # nf-md-file_document
        "Activity Monitor") echo "" ;;       # nf-md-gauge
        *) echo "" ;;                         # nf-md-circle (default)
    esac
}

focused=$(aerospace list-workspaces --focused | tr -d '\n')
previous="aerospace"

for sid in $(aerospace list-workspaces --all); do
    count=$(aerospace list-windows --workspace $sid | wc -l | tr -d ' ')
    if [[ "$focused" == "$sid" || "$count" != "0" ]]; then
        color=0xffffffff
        if [[ "$focused" == "$sid" ]]; then
            color=0xff66ff66
        fi
        
        # Get the first window on this workspace
        app_name=$(aerospace list-windows --workspace $sid | head -n1 | cut -d'|' -f2 | xargs)
        app_icon=""  # default icon
        
        if [[ -n "$app_name" ]]; then
          app_icon=$(get_app_icon "$app_name")
        fi
        
        # Display workspace number with app icon
        label="$sid $app_icon"
        
        sketchybar --add item space.$sid left \
                   --set space.$sid \
                   label.font="Hack Nerd Font:Regular:15.0" \
                   background.color=0x44ffffff \
                   background.corner_radius=5 \
                   background.height=20 \
                   background.drawing=off \
                   label="$label" \
                   label.color=$color \
                   click_script="aerospace workspace $sid" \
                   script="$CONFIG_DIR/plugins/aerospace.sh $sid"
        sketchybar --move space.$sid after "$previous"
        previous="space.$sid"
    else
        sketchybar --remove space.$sid
    fi
done
