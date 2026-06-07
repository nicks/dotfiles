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
    "Ghostty") echo "" ;;                # nf-dev-terminal
    "Emacs") echo "" ;;                   # nf-custom-emacs
    "Neovim") echo "" ;;                  # nf-dev-vim
    "Visual Studio Code") echo "" ;;     # nf-md-microsoft_visual_studio_code
    "Xcode") echo "" ;;                   # nf-dev-xcode
    "Slack") echo "" ;;                   # nf-md-slack
    "Linear") echo "󰗝" ;;                 # nf-md-vector_triangle
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
    "Zoom") echo "" ;;                    # nf-md-video-camera
    "zoom.us") echo "" ;;                 # nf-md-video-camers
    "Telegram") echo "" ;;                # nf-md-telegram
    "WhatsApp") echo "" ;;               # nf-md-whatsapp
    "Preview") echo "" ;;                 # nf-md-image
    "TextEdit") echo "" ;;                # nf-md-file_document
    "Activity Monitor") echo "" ;;       # nf-md-gauge
    Flash*) echo "" ;;      
    *) echo "" ;;                         # nf-md-circle (default)
  esac
}

# Iterate a fixed numeric range so we still clean up sketchybar items
# for workspaces 10+ after they empty out (they drop from `list-workspaces --all`).
MAX_WORKSPACE=30

# Visible workspace per monitor. Missing monitors yield empty strings, which
# never match a numeric workspace id below.
ws_mon_1=$(aerospace list-workspaces --monitor 1 --visible 2>/dev/null | tr -d '\n ')
ws_mon_2=$(aerospace list-workspaces --monitor 2 --visible 2>/dev/null | tr -d '\n ')
ws_mon_3=$(aerospace list-workspaces --monitor 3 --visible 2>/dev/null | tr -d '\n ')

previous="aerospace"

for ((sid=1; sid<=MAX_WORKSPACE; sid++)); do
  count=$(aerospace list-windows --workspace $sid 2>/dev/null | wc -l | tr -d ' ')

  # First match wins, so a workspace claimed by multiple monitors takes the
  # lower-numbered monitor's color.
  color=0xffffffff
  is_visible=false
  if [[ -n "$ws_mon_1" && "$sid" == "$ws_mon_1" ]]; then
    color=0xff66ff66  # bright green: monitor 1
    is_visible=true
  elif [[ -n "$ws_mon_2" && "$sid" == "$ws_mon_2" ]]; then
    color=0xff7dcfff  # cyan: monitor 2
    is_visible=true
  elif [[ -n "$ws_mon_3" && "$sid" == "$ws_mon_3" ]]; then
    color=0xffff9933  # orange: monitor 3
    is_visible=true
  fi

  if [[ "$is_visible" == true || "$count" != "0" ]]; then
    
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
               label.font="FiraCode Nerd Font:Regular:15.0" \
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
