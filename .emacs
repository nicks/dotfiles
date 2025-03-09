;;; .emacs

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

(setq package-list
      '(flycheck
        tide
        vterm
        go-mode
        rust-mode
        flycheck-rust
        typescript-mode
        web-mode
        terraform-mode
        eslint-rc
        lsp-mode
        dockerfile-mode
        protobuf-mode
        python-mode
        company
        lua-mode
        lsp-treemacs
        dotenv-mode
        gptel
        docker-compose-mode))

; activate all the packages
(package-initialize)

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

; install github copilot hooks
(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
            :rev :newest
            :branch "main"))

(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)

; install aidermacs
(use-package aidermacs
  :vc (:url "https://github.com/MatthewZMD/aidermacs" :rev :newest)
  :bind (("C-c p" . aidermacs-transient-menu))

  :config
  ; Enable minor mode for Aider files
  (aidermacs-setup-minor-mode)
  ;; Use vterm backend (default is comint)
  (setq aidermacs-backend 'vterm))

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

; protobuf mode
(autoload 'protobuf-mode "protobuf-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.proto$" . protobuf-mode))

; go mode
(autoload 'go-mode "go-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.go$" . go-mode))
(setq gofmt-command "goimports")
(setq gofmt-args '("-local" "github.com/tilt-dev"))

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

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

(with-eval-after-load 'rust-mode
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; https://github.com/purcell/exec-path-from-shell
;; only need exec-path-from-shell on OSX
;; this hopefully sets up path and other vars better
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize))

(add-hook 'java-mode-hook 'add-untabify-on-write-hook)
(add-hook 'java-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'java-mode-hook 'set-java-indentation-hook)
(add-hook 'makefile-bsdmake-mode-hook 'set-indent-tabs-mode)
(add-hook 'grep-mode-hook 'clear-search-path)
(add-hook 'go-mode-hook 'add-gofmt-hook)
(add-hook 'go-mode-hook 'setup-godef-jump)
(add-hook 'go-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'python-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'yaml-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'lua-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'typescript-mode-hook 'add-trailing-whitespace-on-write-hook)

(defun get-closest-pathname (file)
  "Determine the pathname of the first instance of FILE starting from the current directory towards root.
This may not do the correct thing in presence of links. If it does not find FILE, then it shall return
'/'"
  (get-closest-pathname-helper file (expand-file-name default-directory)))
  
(defun get-closest-pathname-helper (file current-dir)
  (if (file-exists-p (expand-file-name file current-dir))
      current-dir
    (let ((parent-dir (expand-file-name ".." current-dir)))
      (if (>= (length parent-dir) (length current-dir))
          ; (expand-file-name ".." "/") will expand to a longer directory, so stop.
          current-dir
        (get-closest-pathname-helper file parent-dir)))))

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
       (cons "./gradlew" "gradle.properties")
       (cons "yarn" "package.json")
       (cons "go" "go.mod")))

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

(defun smerge-or-next-error ()
  "Run `smerge-next` if in `smerge-mode`, otherwise run `next-error`."
  (interactive)
  (if (and (boundp 'smerge-mode) smerge-mode)
      (smerge-next)
    (next-error)))

(defun smerge-or-prev-error ()
  "Run `smerge-next` if in `smerge-mode`, otherwise run `next-error`."
  (interactive)
  (if (and (boundp 'smerge-mode) smerge-mode)
      (smerge-prev)
    (previous-error)))

; Keyboard shortcuts
(global-set-key "\C-cc" 'compile-command)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cj" 'smerge-or-next-error)
(global-set-key "\C-ck" 'smerge-or-prev-error)
(global-set-key "\C-cw" 'set-size-according-to-resolution)
(global-set-key "\C-cr" 'query-replace-regexp)
(global-set-key "\C-ca" 'gptel-add-file)
(global-set-key "\C-cs" 'gptel-add)
(global-set-key "\C-cd" 'gptel)
(global-set-key "\C-cq" 'gptel-context-remove-all)
(global-set-key "\C-cu" 'smerge-keep-upper)
(global-set-key "\C-cl" 'smerge-keep-lower)
(global-set-key "\C-cm" 'smerge-mode)
(global-set-key "\C-cf" 'grep-find)

; set up auto smerge mode
(defun sm-try-smerge ()
  (save-excursion
  	(goto-char (point-min))
  	(when (re-search-forward "^<<<<<<< " nil t)
  	  (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

(require 'json)

;; Default to utf-8-unix
(prefer-coding-system 'utf-8-unix)
(setq inhibit-eol-conversion t)

;; Colorize compilation buffer.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

(require 'typescript-mode)
(autoload 'typescript-mode "typescript-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.tsx$" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.json$" . typescript-mode))

(require 'python-mode)

(define-derived-mode tiltfile-mode
  python-mode "tiltfile"
  "Major mode for Tilt Dev."
  (setq-local case-fold-search nil))

(add-to-list 'auto-mode-alist '("Tiltfile$" . tiltfile-mode))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
    '(tiltfile-mode . "tiltfile"))

  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection `("tilt" "lsp" "start"))
                     :activation-fn (lsp-activate-on "tiltfile")
                     :server-id 'tilt-lsp)))

(require 'terraform-mode)
(autoload 'terraform-mode "terraform-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.tf$" . terraform-mode))

(require 'lsp-mode)
;;(add-hook 'typescript-mode-hook #'lsp)
;;(add-hook 'web-mode-hook #'lsp)
(add-hook 'tiltfile-mode-hook #'lsp)

;; We usually edit monorepos where file watching won't work well.
(setq lsp-enable-file-watchers nil)

(defun lsp--eslint-before-save (orig-fun)
  "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
  (when lsp-eslint-auto-fix-on-save (lsp-eslint-fix-all))
  (funcall orig-fun))

(advice-add 'lsp--before-save :around #'lsp--eslint-before-save)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/\\1" t)))
 '(backup-directory-alist '((".*" . "~/.emacs.d/backups/")))
 '(c-basic-offset 2)
 '(c-offsets-alist
   '((statement-cont . +) (arglist-intro . c-lineup-arglist-intro-after-paren)))
 '(compilation-environment '("TERM=\"xterm-256color\"" ""))
 '(copilot-indent-offset-warning-disable t)
 '(css-indent-offset 2)
 '(flycheck-disabled-checkers '(go-staticcheck go-golint))
 '(gptel-model 'gpt-4o)
 '(grep-find-command
   '("find . -type f -exec grep --color -nH --null -e  \\{\\} + | cut -c1-\"$COLUMNS\""
     . 49))
 '(lsp-eslint-auto-fix-on-save t)
 '(lsp-eslint-enable t)
 '(lsp-eslint-run "onSave")
 '(package-selected-packages
   '(aidermacs cl docker-compose-mode dockerfile-mode dotenv-mode eslint-rc
               flycheck go-mode lsp-mode protobuf-mode swift-mode terraform-mode
               tide web-mode))
 '(python-indent-offset 2)
 '(safe-local-variable-values
   '((eval let ((project-directory (car (dir-locals-find-file default-directory))))
           (setq lsp-clients-typescript-server-args
                 `("--tsserver-path"
                   ,(concat project-directory
                            ".yarn/sdks/typescript/bin/tsserver")
                   "--stdio")))))
 '(show-paren-mode t)
 '(typescript-indent-level 2)
 '(vc-follow-symlinks t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
