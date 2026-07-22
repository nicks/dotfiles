#!/bin/bash

# Shared Nerd Font app-icon map, sourced by the aerospace and rift
# sketchybar plugins so both render identical icons.

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
