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

# Visible workspace per monitor. Missing monitors yield empty strings, which
# never match a numeric workspace id below.
ws_mon_1=$(aerospace list-workspaces --monitor 1 --visible 2>/dev/null | tr -d '\n ')
ws_mon_2=$(aerospace list-workspaces --monitor 2 --visible 2>/dev/null | tr -d '\n ')
ws_mon_3=$(aerospace list-workspaces --monitor 3 --visible 2>/dev/null | tr -d '\n ')

previous="aerospace"
focused_ws=$(aerospace list-workspaces --focused 2>/dev/null | tr -d '\n ')
existing=$(aerospace list-workspaces --monitor all --empty no | cut -d'|' -f1 | tr -d ' ' | sed 's/^/space./')
current=$(sketchybar --query bar | jq -r '.items[] | select(startswith("space."))')
stale=$(comm -23 <(echo "$current" | sort) <(echo "$existing" | sort))
for item in $stale; do
  sketchybar --remove "$item"
done

for item in $existing; do
  # remove "space." prefix to get workspace id
  sid=$(echo "$item" | sed 's/^space\.//')
  count=$(aerospace list-windows --workspace $sid 2>/dev/null | wc -l | tr -d ' ')

  # First match wins, so a workspace claimed by multiple monitors takes the
  # lower-numbered monitor's color.
  color=0xffffffff
  if [[ -n "$ws_mon_1" && "$sid" == "$ws_mon_1" ]]; then
    color=0xff66ff66  # bright green: monitor 1
  elif [[ -n "$ws_mon_2" && "$sid" == "$ws_mon_2" ]]; then
    color=0xff7dcfff  # cyan: monitor 2
  elif [[ -n "$ws_mon_3" && "$sid" == "$ws_mon_3" ]]; then
    color=0xffff9933  # orange: monitor 3
  fi

  if [[ "$focused_ws" == "$sid" ]]; then
    app_names=$(aerospace list-windows --focused | head -n1 | cut -d'|' -f2 | xargs)
  else
    app_names=$(aerospace list-windows --workspace $sid | sort -n | cut -d'|' -f2)
  fi
  app_icon=""  # default icon

  # app_names are separated by newlines
  while IFS= read -r app_name; do
    # trim space from app_name
    app_name=$(echo "$app_name" | xargs)
    if [[ -z "$app_name" ]]; then
      continue
    fi
    candidate=$(get_app_icon "$app_name")
    echo "$candidate for $app_name"
    if [[ "$candidate" != "" ]]; then
      app_icon="$candidate"
      break
    fi
  done <<< "$app_names"
  
  # Display workspace number with app icon
  label="$sid $app_icon"
  
  sketchybar --add sid space.$sid left \
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
done
