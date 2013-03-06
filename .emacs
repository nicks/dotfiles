
; UI
(transient-mark-mode 1)
(set-background-color "#222")
(set-foreground-color "#ddd")
(set-face-foreground 'modeline "firebrick")
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default fill-column 80)

; shell-mode
(setq path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin")
(setenv "PATH" path)

; backup settings
(setq backup-directory-alist `(("." . "~/.saves")))

; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

(defun add-untabify-on-write-hook ()
  (add-hook 'write-contents-functions 'untabify-buffer nil t))
(defun add-trailing-whitespace-on-write-hook ()
  (add-hook 'write-contents-functions 'delete-trailing-whitespace-hook nil t))

(defun delete-trailing-whitespace-hook ()
  (delete-trailing-whitespace)
  nil)

(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max))
  nil)

(add-hook 'js2-mode-hook 'add-untabify-on-write-hook)
(add-hook 'js2-mode-hook 'add-trailing-whitespace-on-write-hook)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
