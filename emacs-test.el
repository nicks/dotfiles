;;; emacs-test.el --- ERT tests for .emacs configuration -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs --batch -l ~/.emacs -l emacs-test.el -f ert-run-tests-batch-and-exit

;;; Code:

(require 'ert)

;;; Package tests
;; Use `featurep' for packages that are explicitly `require'd in .emacs.
;; Use `package-installed-p' for packages that are autoloaded/lazy-loaded.

(ert-deftest config-test-flycheck-installed ()
  "Flycheck should be installed (autoloaded, not eagerly required)."
  (should (package-installed-p 'flycheck)))

(ert-deftest config-test-magit-installed ()
  "Magit should be installed (autoloaded, not eagerly required)."
  (should (package-installed-p 'magit)))

(ert-deftest config-test-go-mode-installed ()
  "Go mode should be installed (autoloaded via `autoload', not eagerly required)."
  (should (package-installed-p 'go-mode)))

(ert-deftest config-test-exec-path-from-shell-loaded ()
  "exec-path-from-shell should be loaded (explicitly required in .emacs)."
  (should (featurep 'exec-path-from-shell)))

;;; Custom function tests

(ert-deftest config-test-custom-functions-defined ()
  "All custom interactive functions should be defined."
  (should (fboundp 'compile-command))
  (should (fboundp 'format-json))
  (should (fboundp 'format-json-region))
  (should (fboundp 'interactive-rgrep))
  (should (fboundp 'untabify-buffer))
  (should (fboundp 'colorize-compilation-buffer))
  (should (fboundp 'get-closest-pathname))
  (should (fboundp 'get-best-build-descriptor)))

;;; Variable tests

(ert-deftest config-test-variables-set ()
  "Key config variables should have expected values."
  (should (equal gofmt-command "goimports"))
  (should (equal (default-value 'flycheck-temp-prefix) ".flycheck"))
  (should (equal (default-value 'tab-width) 2))
  (should (equal (default-value 'indent-tabs-mode) nil))
  (should (equal (default-value 'fill-column) 80)))

;;; Auto-mode-alist tests

(ert-deftest config-test-auto-mode-alist ()
  "Expected file type associations should be registered."
  (should (assoc "\\.go\\'" auto-mode-alist))
  (should (assoc "\\.proto\\'" auto-mode-alist))
  (should (assoc "\\.tf\\'" auto-mode-alist))
  (should (assoc "\\.ts\\'" auto-mode-alist))
  (should (assoc "\\.tsx\\'" auto-mode-alist))
  (should (assoc "\\.js\\'" auto-mode-alist))
  (should (assoc "\\.jsx\\'" auto-mode-alist))
  (should (assoc "\\.json\\'" auto-mode-alist))
  (should (assoc "Tiltfile\\'" auto-mode-alist)))

;;; Keybinding tests

(ert-deftest config-test-keybindings ()
  "Global keybindings should be set."
  (should (eq (global-key-binding (kbd "C-c c")) 'compile-command))
  (should (eq (global-key-binding (kbd "C-c g")) 'goto-line))
  (should (eq (global-key-binding (kbd "C-c j")) 'next-error))
  (should (eq (global-key-binding (kbd "C-c k")) 'previous-error))
  (should (eq (global-key-binding (kbd "C-c f")) 'interactive-rgrep))
  (should (eq (global-key-binding (kbd "C-c r")) 'query-replace-regexp)))

;;; emacs-test.el ends here
