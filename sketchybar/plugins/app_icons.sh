#!/bin/bash

# Shared Nerd Font app-icon map, sourced by the aerospace and rift
# sketchybar plugins so both render identical icons.

get_app_icon() {
  local app_name="$1"
  case "$app_name" in
    "Google Chrome") echo "" ;;             # nf-dev-chrome
    "Safari") echo "" ;;                    # nf-fa-safari
    "Firefox") echo "" ;;                   # nf-dev-firefox
    "Terminal") echo "" ;;                  # nf-oct-terminal
    "iTerm2") echo "" ;;                    # nf-oct-terminal
    "Alacritty") echo "" ;;                 # nf-oct-terminal
    "Ghostty") echo "" ;;                   # nf-oct-terminal
    "Emacs") echo "" ;;                     # nf-custom-emacs
    "Neovim") echo "" ;;                    # nf-custom-vim
    "Visual Studio Code") echo "󰨞" ;;        # nf-md-microsoft_visual_studio_code
    "Xcode") echo "" ;;                     # nf-dev-xcode
    "Slack") echo "" ;;                     # nf-fa-slack
    "Linear") echo "" ;;                    # nf-fa-vector_square
    "Discord") echo "" ;;                   # nf-fa-discord
    "Spotify") echo "" ;;                   # nf-fa-spotify
    "Docker Desktop") echo "" ;;            # nf-linux-docker
    "Music") echo "" ;;                     # nf-fa-music
    "Finder") echo "󰀶" ;;                    # nf-md-apple_finder
    "System Settings") echo "" ;;           # nf-fa-gear
    "Mail") echo "" ;;                      # nf-fa-envelope
    "Calendar") echo "" ;;                  # nf-fa-calendar_days
    "Notes") echo "" ;;                     # nf-fa-sticky_note
    "Photoshop") echo "" ;;                 # nf-seti-photoshop
    "Figma") echo "" ;;                     # nf-fa-figma
    "Zoom") echo "" ;;                      # nf-fa-video_camera
    "zoom.us") echo "" ;;                   # nf-fa-video_camera
    "Telegram") echo "" ;;                  # nf-fae-telegram
    "WhatsApp") echo "" ;;                  # nf-fa-whatsapp
    "Preview") echo "" ;;                   # nf-fa-picture_o
    "TextEdit") echo "" ;;                  # nf-fa-pencil_square_o
    "Activity Monitor") echo "" ;;          # nf-fa-gauge_high
    Flash*) echo "" ;;                      # nf-fa-flash
    *) echo "" ;;                           # nf-fa-circle
  esac
}
