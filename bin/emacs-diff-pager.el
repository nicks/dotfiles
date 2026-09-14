;;; emacs-diff-pager.el --- render a piped diff with magit  -*- lexical-binding: t -*-

;;; Commentary:

;; Loaded by the `emacs-diff-pager' script sitting next to this file, which
;; spools a unified diff off its stdin and then runs
;;
;;     emacs -nw -l emacs-diff-pager.el -f emacs-diff-pager-show
;;
;; with EMACS_DIFF_PAGER_FILE and EMACS_DIFF_PAGER_ROOT set.  The inputs travel
;; through the environment so that neither the paths nor this file's source has
;; to survive a trip through shell quoting.

;;; Code:

(require 'magit)

(defun emacs-diff-pager--strip-prefixes ()
  "Strip a/ and b/ path prefixes from the diff in the current buffer.
Magit always runs git with `--no-prefix', so its washing code only knows how
to parse diffs whose paths carry no prefix."
  (goto-char (point-min))
  (while (re-search-forward "^diff --git \\(.*\\)$" nil t)
    (replace-match
     (concat "diff --git "
             (replace-regexp-in-string "\\(\\`\\| \\)\\(\"?\\)[ab]/" "\\1\\2"
                                       (match-string 1)))
     t t)
    ;; Only touch the ---/+++ lines of this file's header, so that content
    ;; lines inside a hunk (e.g. when diffing a patch file) are left alone.
    ;; A binary file has no hunk, so the next file's header bounds the search
    ;; too; without it the scan runs on into later files.  `save-excursion'
    ;; keeps that scan from carrying the outer loop past the headers it
    ;; crossed, which would leave them unstripped.
    (save-excursion
      (let ((limit (save-excursion
                     (if (re-search-forward "^\\(?:@@\\|diff --git \\)" nil t)
                         (match-beginning 0)
                       (point-max)))))
        (while (re-search-forward "^\\(---\\|\\+\\+\\+\\) \\(\"?\\)[ab]/" limit t)
          (replace-match "\\1 \\2"))))))

(defun emacs-diff-pager-show (&optional file dir)
  "Display the unified diff in FILE in a magit diff buffer, filling the frame.
DIR is the repo root the diff's paths are relative to, so that RET on a hunk
visits the right file.  Both default to EMACS_DIFF_PAGER_FILE and
EMACS_DIFF_PAGER_ROOT, which is how the pager script passes them."
  (let* ((file (or file (getenv "EMACS_DIFF_PAGER_FILE")))
         (dir (file-name-as-directory (or dir (getenv "EMACS_DIFF_PAGER_ROOT"))))
         (buffer (get-buffer-create
                  (format "*diff: %s*"
                          (file-name-nondirectory (directory-file-name dir))))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (magit-diff-mode)
        ;; `default-directory' is a permanent local, so it survives the major
        ;; mode change above.
        (setq default-directory dir)
        ;; The diff came in over a pipe rather than from a magit command, so
        ;; there is no range behind it; `undefined' tells magit's visit-file
        ;; machinery to jump to the worktree version of the file.
        (setq-local magit-buffer-diff-type 'undefined)
        (insert-file-contents file)
        (emacs-diff-pager--strip-prefixes)
        (goto-char (point-min))
        (magit-insert-section (diffbuf)
          (magit-diff-wash-diffs nil))
        (goto-char (point-min)))
      ;; This Emacs exists only to show the diff, so `q' should hand the
      ;; terminal back rather than bury the buffer.  A fresh map, because
      ;; `local-set-key' would edit `magit-diff-mode-map' itself.
      (let ((map (make-sparse-keymap)))
        (set-keymap-parent map (current-local-map))
        (define-key map (kbd "q") #'save-buffers-kill-terminal)
        (use-local-map map)))
    (switch-to-buffer buffer)
    (delete-other-windows)))

(provide 'emacs-diff-pager)

;;; emacs-diff-pager.el ends here
