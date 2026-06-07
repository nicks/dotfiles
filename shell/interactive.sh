# Shared interactive shell setup — POSIX-compatible.
# `command emacs` bypasses the alias defined in aliases.sh.
emacsclient -a false -e 't' >/dev/null 2>&1 || command emacs --daemon >/dev/null 2>&1 &
