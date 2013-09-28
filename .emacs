(require 'cl)
(require 'compile)

(add-to-list 'load-path "~/emacs/lisp")

; UI
(transient-mark-mode 1)
(set-background-color "#222")
(set-foreground-color "#ddd")
(set-face-foreground 'mode-line "firebrick")
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default fill-column 80)

; shell-mode
(setq path "/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/usr/local/git/bin:/Library/Java/JavaVirtualMachines/jdk1.7.0_12.jdk/Contents/Home/bin")
(setenv "PATH" path)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

; backup settings
(setq backup-directory-alist `(("." . "~/.saves")))

; js2-mode
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; less-mode
(autoload 'less-css-mode "less-css-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.less$" . less-css-mode))

; closure-template-html-mode
(autoload 'closure-template-html-mode "closure-template-html-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.soy$" . closure-template-html-mode))

; protobuf mode
(autoload 'protobuf-mode "protobuf-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

; go mode
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))

; manage-imports
(autoload 'import-word "manage-imports" t nil)

(defun add-untabify-on-write-hook ()
  (add-hook 'write-contents-functions 'untabify-buffer nil t))
(defun add-trailing-whitespace-on-write-hook ()
  (add-hook 'write-contents-functions 'delete-trailing-whitespace-hook nil t))

(defun delete-trailing-whitespace-hook ()
  (save-excursion (delete-trailing-whitespace))
  nil)

(defun untabify-buffer ()
  (interactive)
  (save-excursion (untabify (point-min) (point-max)))
  nil)

(defun clear-search-path ()
  (set (make-local-variable 'compilation-search-path) '(nil))
  nil)

(defun set-indent-tabs-mode ()
  (set (make-local-variable 'intend-tabs-mode) t)
  nil)

(setq-default indent-tabs-mode nil)

(defun add-gofmt-hook ()
  (add-hook 'before-save-hook 'gofmt-before-save))

(defun setup-godef-jump ()
  (local-set-key (kbd "M-.") 'godef-jump))

(add-hook 'js2-mode-hook 'add-untabify-on-write-hook)
(add-hook 'js2-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'java-mode-hook 'add-untabify-on-write-hook)
(add-hook 'java-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'makefile-bsdmake-mode-hook 'set-indent-tabs-mode)
(add-hook 'closure-template-html-mode-hook 'add-untabify-on-write-hook)
(add-hook 'closure-template-html-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'grep-mode-hook 'clear-search-path)
(add-hook 'go-mode-hook 'add-gofmt-hook)
(add-hook 'go-mode-hook 'setup-godef-jump)

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
			return ""))))

(defun compile-command ()
  (interactive)
  (set (make-local-variable 'compile-command) 
       (format "cd %s && make %s" 
	       (get-closest-pathname)
	       (let* ((last-command (car compile-history))
		      (make-command-index (string-match "make .*" (or last-command ""))))
		 (if make-command-index
		     (substring last-command (+ make-command-index 5))
		   ""))))
  (call-interactively 'compile))

; Window resolution
(defun set-size-according-to-resolution ()
  (interactive)
  (if (display-graphic-p)
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (if (> (x-display-pixel-width) 1280)
           (add-to-list 'default-frame-alist (cons 'width 120))
           (add-to-list 'default-frame-alist (cons 'width 80)))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist 
         (cons 'height (/ (- (x-display-pixel-height) 200)
                             (frame-char-height)))))))

(set-face-attribute 'default nil :height 144)
(set-size-according-to-resolution)

; yasnippet.el
(add-to-list 'load-path "~/.emacs.d/plugins/yasnippet")
(require 'yasnippet)
(yas-global-mode 1)

; Keyboard shortcuts
(global-set-key "\C-cc" 'compile-command)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cj" 'next-error)
(global-set-key "\C-ck" 'previous-error)
(global-set-key "\C-cw" 'set-size-according-to-resolution)
(global-set-key "\C-cy" 'yas/insert-snippet)
(global-set-key "\C-cv" 'recompile)
(global-set-key "\C-ci" 'import-word)

(require 'json)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 2)
 '(c-offsets-alist (quote ((statement-cont . 4) (arglist-intro . 4))))
 '(js2-basic-offset 2)
 '(js2-strict-missing-semi-warning nil)
 '(js2-bounce-indent-p t)
 '(js2-continuation-offset 4)
 '(js2-strict-missing-semi-warning nil)
 '(less-css-compile-at-save nil)
 '(less-css-lessc-command "lessc")
 '(less-css-lessc-options nil)
 '(python-indent-offset 2)
 '(show-paren-mode t)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
