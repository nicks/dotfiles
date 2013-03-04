
; UI
(transient-mark-mode 1)
(set-background-color "#222")
(set-foreground-color "#ddd")
(set-face-foreground 'modeline "firebrick")

; shell-mode
(setq path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin")
(setenv "PATH" path)

; backup settings
(setq backup-directory-alist `(("." . "~/.saves")))
