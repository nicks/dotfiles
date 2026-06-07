# Bash login shells skip .profile when this file exists, so source it explicitly.
# Then source .bashrc so interactive features work in login terminals too.
[ -r "$HOME/.profile" ] && . "$HOME/.profile"
[ -r "$HOME/.bashrc" ] && . "$HOME/.bashrc"
