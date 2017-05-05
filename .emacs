;;; .emacs

(require 'cl)
(require 'compile)
(require 'package)

(package-initialize)

(require 'flycheck)

(defun package-require (pkg)
  "Install a package only if it's not already installed."
  (when (not (package-installed-p pkg))
    (package-install pkg)))

(add-to-list 'load-path "~/emacs/lisp")

; setup elpa
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.org/packages/")))

; UI
(transient-mark-mode 1)
(set-background-color "#222")
(set-foreground-color "#ddd")
(set-face-foreground 'mode-line "firebrick")
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default fill-column 80)
(setq-default tab-width 2)
(setq ring-bell-function 'ignore)


(setq find-args "-name .git -prune -o -name node_modules -prune -o -name out -prune -o -name .svn -prune -o -type f ! -name '*class'")

; shell-model
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

; js2-mode
(package-require 'js2-mode)

(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("BUILD" . python-mode))
(add-to-list 'auto-mode-alist '("BUCK" . python-mode))
(add-to-list 'auto-mode-alist '("\\.bzl$" . python-mode))

(setq-default js2-mode-indent-ignore-first-tab t)
(setq-default js2-mode-show-parse-errors nil)
(setq-default js2-strict-inconsistent-return-warning nil)
(setq-default js2-strict-var-hides-function-arg-warning nil)
(setq-default js2-strict-missing-semi-warning nil)
(setq-default js2-strict-trailing-comma-warning nil)
(setq-default js2-strict-cond-assign-warning nil)
(setq-default js2-strict-var-redeclaration-warning nil)
(setq-default js2-global-externs
      '("module" "require" "__dirname" "process" "console" "define" "JSON"))

; jshint
(setq jshint-error-regexp 
      '("^\\([a-zA-Z\.0-9_/-]+\\): line \\([0-9]+\\), col \\([0-9]+\\)" 1 2 3))
(setq compilation-error-regexp-alist
      (cons jshint-error-regexp compilation-error-regexp-alist))

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
(package-require 'go-mode)
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(setq gofmt-command "goimports")

; coffee mode
(autoload 'coffee-mode "coffee-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))

; manage-imports
(autoload 'import-word "manage-imports" t nil)
(autoload 'sort-imports "manage-imports" t nil)
(autoload 'load-java-imports-file "manage-imports" t nil)
(load-java-imports-file "~/.emacs-java-imports")

;; json formatting
;; http://irreal.org/blog/?p=354
(defun format-json ()
  (interactive)
  (shell-command-on-region (point-min) (point-max) "python -m json.tool" (buffer-name) t)
  nil)

(defun format-json-region ()
  (interactive)
  (shell-command-on-region (mark) (point) "python -m json.tool" (buffer-name) t)
  nil)

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

(defun set-java-indentation-hook ()
  (if (string-match "android" (buffer-file-name))
      (set (make-local-variable 'c-basic-offset) 4)
    nil))

;; turn on flychecking globally
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; disable pyhint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(python-pylint)))

(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(protobuf-protoc)))

;; use eslint with web-mode for jsx files
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-mode 'javascript-eslint 'js2-mode)
(flycheck-add-mode 'javascript-eslint 'javascript-mode)

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(json-jsonlist)))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'js2-mode-hook (lambda () (set (make-local-variable 'electric-indent-chars) '(?\n))))
(add-hook 'js2-mode-hook 'add-untabify-on-write-hook)
(add-hook 'js2-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'java-mode-hook 'add-untabify-on-write-hook)
(add-hook 'java-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'java-mode-hook 'set-java-indentation-hook)
(add-hook 'makefile-bsdmake-mode-hook 'set-indent-tabs-mode)
(add-hook 'closure-template-html-mode-hook 'add-untabify-on-write-hook)
(add-hook 'closure-template-html-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'grep-mode-hook 'clear-search-path)
(add-hook 'go-mode-hook 'add-gofmt-hook)
(add-hook 'go-mode-hook 'setup-godef-jump)
(add-hook 'go-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'python-mode-hook 'add-trailing-whitespace-on-write-hook)

; Default compile commands.
; Find the nearest makefile and use that.
; Courtesy of http://emacswiki.org/emacs/CompileCommand#toc5
(defun get-closest-pathname (file)
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return 
'/'"
  (let ((root (expand-file-name "/"))) ; the win32 builds should translate this correctly
    (expand-file-name (loop 
			for d = default-directory then (expand-file-name ".." d)
			if (file-exists-p (expand-file-name file d))
			return d
			if (equal d root)
			return "/"))))

(defun max-element (arr scorer)
  (defun helper(rest current-max)
    (if (car rest)
        (helper (cdr rest)
                (if (> (funcall scorer (car rest)) (funcall scorer current-max))
                    (car rest) current-max))
      current-max))
  (helper (cdr arr) (car arr)))

(setq build-descriptors
      (list
       (cons "grunt" "Gruntfile.js")
       (cons "mvn" "pom.xml")
       (cons "make" "Makefile")
       (cons "buck" "BUCK")
       (cons "./gradlew" "gradle.properties")))

(defun get-best-build-descriptor ()
  (max-element build-descriptors
               (lambda (desc)
                 (length (get-closest-pathname (cdr desc))))))

(defun compile-command ()
  (interactive)
  (set (make-local-variable 'compile-command)
       (let* ((desc (get-best-build-descriptor))
              (last-command (car compile-history))
              (make-command-index
               (string-match
                (format "%s .*" (car desc))
                (or last-command ""))))
         (format "cd %s && %s %s"
                 (get-closest-pathname (cdr desc)) ; path
                 (car desc) ; command
                 (if make-command-index
                     (substring last-command (+ make-command-index (length (car desc)) 1))
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

(set-face-attribute 'default nil :height 160)
(set-size-according-to-resolution)

; Keyboard shortcuts
(global-set-key "\C-cc" 'compile-command)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cj" 'next-error)
(global-set-key "\C-ck" 'previous-error)
(global-set-key "\C-cw" 'set-size-according-to-resolution)
(global-set-key "\C-cy" 'yas/insert-snippet)
(global-set-key "\C-cv" 'recompile)
(global-set-key "\C-ci" 'import-word)
(global-set-key "\C-cs" 'sort-imports)

(require 'json)

;; Default to utf-8-unix
(prefer-coding-system 'utf-8-unix)
(setq inhibit-eol-conversion t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms (quote ((".*" "~/.emacs.d/autosaves/\\1" t))))
 '(backup-directory-alist (quote ((".*" . "~/.emacs.d/backups/"))))
 '(c-basic-offset 2)
 '(c-offsets-alist (quote ((statement-cont . 4) (arglist-intro . 4))))
 '(compilation-search-path (quote ("." "global-shared" "out/intermediates/web-fe")))
 '(css-indent-offset 2)
 '(flycheck-javascript-eslint-executable "medium2-lint.sh")
 '(js2-basic-offset 2)
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
