(require 'cl)
(require 'compile)

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
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

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

; Default compile commands.
; Find the nearest makefile and use that.
; Courtesy of http://emacswiki.org/emacs/CompileCommand#toc5
(defun* get-closest-pathname (&optional (file "Makefile"))
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return the name
of FILE in the current directory, suitable for creation"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name (loop 
			for d = default-directory then (expand-file-name ".." d)
			if (file-exists-p (expand-file-name file d))
			return d
			if (equal d root)
			return nil))))

(defun compile-command ()
  (interactive)
  (set (make-local-variable 'compile-command) (format "cd %s && make " (get-closest-pathname)))
  (call-interactively 'compile))

; Keyboard shortcuts
(global-set-key "\C-cc" 'compile-command)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cj" 'next-error)
(global-set-key "\C-ck" 'previous-error)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js2-basic-offset 2)
 '(js2-strict-missing-semi-warning nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
