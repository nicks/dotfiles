(require 'cl-lib)
(require 'package)

;; add melpa to package sources
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))

;; packages to install
(setq package-list
      '(flycheck
        tide
        vterm
        go-mode
        rust-mode
        flycheck-rust
        typescript-ts-mode
        web-mode
        terraform-mode
        eslint-rc
        lsp-mode
        dockerfile-mode
        protobuf-mode
        python-mode
        company
        lua-mode
        dotenv-mode
        gptel
        aider
        forge
        magit
        exec-path-from-shell
        sqlite3
        docker-compose-mode
        rego-mode))

;; activate all the packages
(package-initialize)

;; install the missing packages
(if
    (cl-some (lambda (x) (not (package-installed-p x))) package-list)
    (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; install github copilot hooks
(require 'vc-git)
(use-package copilot
  :ensure t
  :defer t
  :hook (prog-mode . copilot-mode)
  :bind (:map copilot-completion-map
              ("<tab>" . copilot-accept-completion)
              ("TAB" . copilot-accept-completion)
              ("C-<tab>" . copilot-accept-completion-by-word)
              ("C-TAB" . copilot-accept-completion-by-word))
  :init
  (setq copilot-indent-offset-warning-disable t))

;; default to text mode for new buffers.
;; by default, emacs loads new buffers in a lisp mode,
;; which triggers the copilot hooks and causes a long startup time.
(setq initial-major-mode 'text-mode)

;; authinfo creds for forge
(setq auth-sources '("~/.authinfo"))

;; UI
(add-to-list 'default-frame-alist '(tool-bar-lines . t))
(add-to-list 'default-frame-alist '(menu-bar-lines . t))
(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
(add-to-list 'initial-frame-alist '(tool-bar-lines . t))
(add-to-list 'initial-frame-alist '(menu-bar-lines . t))
(add-to-list 'initial-frame-alist '(ns-transparent-titlebar . t))

(transient-mark-mode 1)
(set-background-color "#0e0f14")
(set-foreground-color "#ffffff")
(set-face-background 'mode-line "#15161e")
(set-face-foreground 'mode-line "#7aa2f7")
(setq line-number-mode t)
(setq column-number-mode t)
(setq-default fill-column 80)
(setq-default tab-width 2)
(setq ring-bell-function 'ignore)

(setq find-args "-name .git -prune -o -name node_modules -prune -o -name out -prune -o -name .svn -prune -o -type f ! -name '*class'")

;; follow symlinks automatically
(setq vc-follow-symlinks t)

;; shell-model
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)
(add-hook 'compilation-mode-hook 'ansi-color-for-comint-mode-on)

;; emacs-plus sets CC to homebrew gcc, which breaks go builds.
;; we want to use the system clang instead.
(setenv "CC" "")
(setenv "LIBRARY_PATH" "")

;; create the autosave dir if necessary, since emacs won't.
(make-directory "~/.emacs.d/autosaves/" t)

;; protobuf mode
(use-package protobuf-mode
  :mode "\\.proto\\'")

;; rego mode
(use-package rego-mode
  :mode "\\.rego\\'")

;; go mode
(use-package go-mode
  :mode "\\.go\\'"
  :init
  (setq gofmt-command "goimports")
  (setq gofmt-args '("-local" "github.com/tilt-dev")))

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
(require 'exec-path-from-shell)
(when (memq window-system '(mac ns))
  (run-with-idle-timer 1 nil #'exec-path-from-shell-initialize))

(add-hook 'java-mode-hook 'add-untabify-on-write-hook)
(add-hook 'java-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'java-mode-hook 'set-java-indentation-hook)
(add-hook 'makefile-bsdmake-mode-hook 'set-indent-tabs-mode)
(add-hook 'go-mode-hook 'add-gofmt-hook)
(add-hook 'go-mode-hook 'setup-godef-jump)
(add-hook 'go-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'python-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'yaml-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'lua-mode-hook 'add-trailing-whitespace-on-write-hook)
(add-hook 'typescript-ts-mode-hook 'add-trailing-whitespace-on-write-hook)

;; grep customization
(with-eval-after-load 'grep
  (add-to-list 'grep-find-ignored-directories ".yarn")
  (add-to-list 'grep-find-ignored-directories ".claude")
  (add-to-list 'grep-find-ignored-directories ".specstory")
  (add-to-list 'grep-find-ignored-directories ".terraform")
  (add-to-list 'grep-find-ignored-directories "node_modules")
  (add-to-list 'grep-find-ignored-directories "vendor"))

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

(set-face-attribute 'default nil :height 160)
(set-frame-font "FiraCode Nerd Font" nil t)

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

;; grep functions
(defun interactive-rgrep (pattern)
  "Interactive grep with pattern input, searching all files in current buffer's directory.
PATTERN is the search pattern to use with rgrep."
  (interactive
   (list (read-string "Search pattern: ")))
  (let ((default-directory (file-name-directory (or (buffer-file-name) default-directory))))
    (rgrep pattern "*" default-directory)))

;; Keyboard shortcuts
(global-set-key "\C-cc" 'compile-command)
(global-set-key "\C-cg" 'goto-line)
(global-set-key "\C-cj" 'smerge-or-next-error)
(global-set-key "\C-ck" 'smerge-or-prev-error)
(global-set-key "\C-cr" 'query-replace-regexp)
(global-set-key "\C-cd" 'gptel)
(global-set-key "\C-cu" 'smerge-keep-upper)
(global-set-key "\C-cl" 'smerge-keep-lower)
(global-set-key "\C-cm" 'smerge-mode)
(global-set-key "\C-cf" 'interactive-rgrep)
(global-set-key "\C-ca" 'aider-run-aider)

;; copy and paste
;; cmd-c and cmd-v are objectively better copy/paste shortcuts
(global-set-key (kbd "<XF86Paste>") 'yank)
(global-set-key (kbd "<XF86Copy>") 'kill-ring-save)
(global-set-key (kbd "<XF86Cut>") 'kill-region)

(setq gptel-model 'gpt-4.1
      gptel-backend (gptel-make-gh-copilot "Copilot"))

;; set up auto smerge mode
(defun sm-try-smerge ()
  (save-excursion
  	(goto-char (point-min))
  	(when (re-search-forward "^<<<<<<< " nil t)
  	  (smerge-mode 1))))
(add-hook 'find-file-hook 'sm-try-smerge t)

(require 'json)

;;; Default to utf-8-unix
(prefer-coding-system 'utf-8-unix)
(setq inhibit-eol-conversion t)

;; Colorize compilation buffer.
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point-max)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; modern typescript mode
(use-package typescript-ts-mode
  :mode (("\\.js\\'" . typescript-ts-mode)
         ("\\.jsx\\'" . tsx-ts-mode)
         ("\\.ts\\'" . typescript-ts-mode)
         ("\\.tsx\\'" . tsx-ts-mode)
         ("\\.json\\'" . tsx-ts-mode))
  :defer t)

(setq treesit-language-source-alist
    '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
      (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")))

(require 'treesit)
(dolist (lang '(typescript tsx))
  (unless (treesit-ready-p lang)
    (treesit-install-language-grammar lang)))

(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)

(use-package python-mode
  :mode "\\.py\\'"
  :defer t)
(define-derived-mode tiltfile-mode
  python-mode "tiltfile"
  "Major mode for Tilt Dev."
  (setq-local case-fold-search nil))

(add-to-list 'auto-mode-alist '("Tiltfile\\'" . tiltfile-mode))

(use-package terraform-mode
  :mode "\\.tf\\'")

(use-package lsp-mode
  :hook (tiltfile-mode . lsp)
  :config
  (add-to-list 'lsp-language-id-configuration '(tiltfile-mode . "tiltfile"))
  (lsp-register-client
    (make-lsp-client :new-connection (lsp-stdio-connection `("tilt" "lsp" "start"))
                     :activation-fn (lsp-activate-on "tiltfile")
                     :server-id 'tilt-lsp))
  ;; We usually edit monorepos where file watching won't work well.
  (setq lsp-enable-file-watchers nil)
  (defun lsp--eslint-before-save (orig-fun)
    "Run lsp-eslint-apply-all-fixes and then run the original lsp--before-save."
    (when lsp-eslint-auto-fix-on-save (lsp-eslint-fix-all))
    (funcall orig-fun))
  (advice-add 'lsp--before-save :around #'lsp--eslint-before-save))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(c-basic-offset 'set-from-style)
 '(package-selected-packages
   '(aider claude-code company copilot docker-compose-mode dockerfile-mode
           dotenv-mode eat eslint-rc exec-path-from-shell flycheck-rust forge
           go-mode gptel lsp-mode lua-mode protobuf-mode python-mode rego-mode
           rust-mode sqlite3 terraform-mode tide typescript-mode vterm web-mode))
 '(package-vc-selected-packages
   '((claude-code :url "https://github.com/stevemolitor/claude-code.el")
     (copilot :url "https://github.com/copilot-emacs/copilot.el" :branch "main")))
 '(sh-basic-offset 2)
 '(typescript-ts-mode-indent-offset 2))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'dired-find-alternate-file 'disabled nil)
