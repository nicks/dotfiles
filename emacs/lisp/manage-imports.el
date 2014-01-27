;; manage-imports.el
;; Author: nicholas.j.santos@gmail.com (Nick Santos)
;;
;; An emacs package for managing import statements.
;; Supports both JS goog.requires and Java imports

(require 'cl)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Collecting and analyzing imports

(defun java-imports-mode ()
  (equal major-mode 'java-mode))

(defun javascript-imports-mode ()
  (or (equal major-mode 'html-mode)
      (equal major-mode 'javascript-mode)
      (equal major-mode 'js2-mode)))

(defun imports-unsupported-error ()
  (error "No import symtax defined for major mode '%s'" major-mode))

(defun imports-regexp ()
  "Returns a regexp that matches import statements.

  The first group of the regexp will also match the text of the import."
  (cond ((java-imports-mode)
	 "\n\\(?:static \\)?import \\([^;]+\\);")
	((javascript-imports-mode)
	 "\n *goog\\.require([\"']\\([^\"']+\\)[\"'])")
	  (t (imports-unsupported-error))))

(defun imports-format ()
  "Returns a format string for imports.

  The format string should have a placeholder for one string:
  the imported symbol."
  (cond ((java-imports-mode)
	 "import %s;\n")
	((javascript-imports-mode)
	 "goog.require('%s');\n")
	(t (imports-unsupported-error))))

(defun categories-of-imports ()
  "Returns a list of all the possible categories of imports."
  ; must be in-sync with category-of-import
  (if (java-imports-mode) '(1 2 3) '(1)))

(defun category-of-import (import-name)
  "Determines the category of an imported name.
   
   Args:
     import-name: The name of the imported symbol.
   Returns:
     A number signifying the category of the import. Imports with lower
     numbers should appear before imports with higher numbers."
  (if (equal major-mode 'java-mode)
      ; in Java, it goes Google < third party < Java libraries
      (if (string-match "^com\\.google" import-name)
	  1
	(if (string-match "^java\\." import-name)
	    3
	  2))
    1))

(defun imports-of-category (imports-list category)
  "Returns all the imports in the given list that match the given category.
  
   Args:
     imports-list: A list of strings.
     category: A number as returned by category-of-import.
   Return:
     A list of strings."
  (let ((result '()))
    (while (not (null imports-list))
	   (when (= category (category-of-import (car imports-list)))
	     (setq result (cons (car imports-list) result)))
	   (setq imports-list (cdr imports-list)))
    result))

(defun sort-and-categorize-imports (imports-list)
  "Sorts imports into categories.

   Args:
     imports-list: A list of strings.
   Returns:
     A list of lists of strings, like (('foo' 'bar') ('baz')). Imports
     in the same inner list are in the same category. In the outer list,
     elements are sorted by category. In the inner list, the strings are
     alphabetized."
  (let ((mapFn 
	 (lambda (c) (sort (imports-of-category imports-list c) 'string<))))
    (delq '() (mapcar mapFn (categories-of-imports)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Reading imports from a buffer

(defun imports-header-skip ()
  "Move point past statements before the imports
   like comments and package statements."
  (progn
    (search-forward "\n\n" nil t)
    (previous-line)
    (when (and (javascript-imports-mode)
	       (search-forward "\ngoog.provide" nil t))
      (while (search-forward "\ngoog.provide" nil t))
      (next-line))
    (if (java-imports-mode)
	(and (search-forward "\npackage" nil t) (next-line)))))

(defun beginning-of-imports ()
  "Move point to the beginning of the imports block."
  (interactive)
  (progn
    (goto-char (point-min))
    (unless (re-search-forward (imports-regexp) nil t)
      (imports-header-skip))
    (beginning-of-line)))

(defun end-of-imports ()
  "Move point to the end of the imports block."
  (interactive)
  (progn 
    (goto-char (point-max))
    (if (re-search-backward (imports-regexp) nil t)
	(progn (forward-line) (end-of-line))
      (beginning-of-imports))))

(defun imports-after-point ()
  "Finds all the imports after the current point."
  (save-excursion
    (let ((result '()))
      (while (re-search-forward (imports-regexp) nil t)
	(setq result 
	      (cons (match-string-no-properties 1) result)))
      result)))

(defun qualified-name-at-point ()
  "Finds the current qualified name at point."
  (save-excursion
    (skip-syntax-backward "w_")
    (while (equal ?. (preceding-char)) (backward-word))
    (let ((start (point)))
      (skip-syntax-forward "w_")
      (while (equal ?. (following-char)) (forward-word))
      (buffer-substring-no-properties start (point)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Abbrev expansion for Java imports
; Doesn't use real apprev tables, because the abbrev manual is unreadable :(

(defvar java-imports-file-name nil)

(defvar java-imports-alist '())

(defun load-java-imports-file (file-name)
  "Bootstrap the Java imports file"
  (progn
    (setq java-imports-file-name file-name)
    (if (file-exists-p file-name)
	(load file-name))))

(defun expand-java-import (word)
  "Turns a word into a fully qualified name for importing."
  (let ((entry (assoc word java-imports-alist)))
    (when (null entry)
      ; Read the new import, and insert it into the alist.
      (setq entry (cons word (read-string (format "Import %s: " word))))
      (setq java-imports-alist (cons entry java-imports-alist))
      
      ; Write the alist to a file, since it has changed.
      (if (null java-imports-file-name)
	  (error "Please call load-java-imports-file!"))
      
      (write-region
       (format "(setq java-imports-alist `%s)" (print java-imports-alist))
       nil java-imports-file-name))
    (cdr entry)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Writing and modifying imports

(defun replace-imports (import-names)
  "Kill all the imports in the buffer and replace them."
  (save-excursion 
    (let ((print-categorized-imports
	   (lambda (list)
	     (progn
	       (mapc 
		(lambda (name) (insert (format (imports-format) name)))
		list)
	       (insert "\n"))))
	  (end (progn (end-of-imports) (point)))
	  (start (progn (beginning-of-imports) (point))))
      ; Delete the old imports
      (delete-region start end)
      ; and print the new ones
      (mapc print-categorized-imports 
	    (sort-and-categorize-imports import-names))
      ; kill the extra line at the end
      (unless (null import-names)
	(kill-line) (kill-line)))))

(defun add-to-imports (new-import)
  "Add an import to the imports at the top of the buffer."
  (save-excursion
    ; Read all the imports
    (goto-char (point-min))
    (let ((current-imports (imports-after-point)))
      (unless (member new-import current-imports)
	(replace-imports (cons new-import current-imports))))))

(defun import-word ()
  "Imports the word at point."
  (interactive)
  (add-to-imports
   (cond ((java-imports-mode)
	  (expand-java-import (current-word)))
	 ((javascript-imports-mode)
	  (qualified-name-at-point))
	 (t (imports-unsupported-error)))))

(defun sort-imports ()
  "Sort the imports at the top of the file"
  (interactive)
  (save-excursion
    ; Read all the imports
    (goto-char (point-min))
    (replace-imports (imports-after-point))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unit test helper functions

(defun funcall-in-mode (mode fn)
  "Eval the given function in the given mode.
  
  Args:
    fn: A lambda that takes no arguments.
    mode: A symbol for a major mode."
  (let ((old-mode major-mode))
     (prog2
	 (funcall (indirect-function mode))
	 (funcall fn)
       (funcall (indirect-function old-mode)))))

(defun import-in-string (str)
  "Returns the import named in the given string."
  (if (string-match (imports-regexp) str)
      (match-string-no-properties 1 str)
    nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Unit tests

(if nil
(eval-when-compile
  ; sort-and-categorize-imports
  (assert (equal nil (sort-and-categorize-imports '())))
  (assert (equal '(("a" "b")) (sort-and-categorize-imports '("a" "b"))))
  (assert (equal '(("a" "b")) (sort-and-categorize-imports '("b" "a"))))
  (assert (funcall-in-mode 
	   'java-mode 
	   (lambda ()
	     (equal 
	      '(("com.google.List") ("javaframework.X") ("java.util.List"))
	      (sort-and-categorize-imports 
	       '("com.google.List" "javaframework.X" "java.util.List"))))))

  ; regexp tests
  (assert (funcall-in-mode 
	   'java-mode 
	   (lambda () 
	     (equal 
	      "com.google.List" 
	      (import-in-string "\nimport com.google.List;")))))
  (assert (funcall-in-mode 
	   'java-mode 
	   (lambda () 
	     (equal 
	      nil 
	      (import-in-string "\nimport com.google.List")))))
  (assert (funcall-in-mode 
	   'html-mode 
	   (lambda () 
	     (equal 
	      "goog.Menu" 
	      (import-in-string "\ngoog.require('goog.Menu');")))))
  (assert (funcall-in-mode 
	   'html-mode 
	   (lambda () 
	     (equal 
	      "goog.MenuBar"
	      (import-in-string "\n   goog.require(\"goog.MenuBar\")")))))
  (assert (funcall-in-mode 
	   'html-mode 
	   (lambda () 
	     (equal 
	      nil
	      (import-in-string "\ngoog.provide('goog.MenuBar')")))))

  ; Done!
  (message "Unit tests passed!"))
) ; end if nil
