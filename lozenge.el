;;; lozenge.el --- Pollen inspired lozenge command syntax            -*- lexical-binding: t; -*-

;; Copyright (C) 2019-- Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/lozenge
;; Version: 1.00
;; Package-Requires: ((emacs "25") (org "9") (xmlgen "0.5") (dash "2.16.0"))

;;; Commentary:
;; Pollen inspired Lozenge-syntax for Emacs.
;; See examples etc in the repo.

;;; Code:
(require 'xmlgen)
(require 'dash)
(require 'org-macs)
(require 'subr-x)

(defvar lozenge-symbol-prefix "loz/"
  "`lozenge' will check for variables and functions with this prefix.")

(defvar lozenge-lozenge-char ?◊
  "The lozenge char.")

(defvar lozenge-half-lozenge-char ?⟠
  "The half-lozenge char.")

(defvar lozenge-in-comment-check-list
  '((org-mode . (or (org-in-commented-heading-p)
                    (org-at-comment-p))))
  "An alist where  each pair is a `major-mode' and an sexp.
The sexp should return a non-nil value if `point' is in a comment.
If the major mode isn't in the list (nth 4 (syntax-ppss))  is used.")

(defvar lozenge-list-processing-functions
  '((org-mode/html . lozenge-list-to-org-html)
    (markdown-mode . lozenge-list-to-html))
  "If `lozenge-eval' returns a list it will be sent to a function in this alist.
Each pair is a `major-mode' and the function to be used. In the
case of `org-mode', a slash and the export backend should be
appended (see default setting). If the `major-mode' isn't found,
the list elements are separated by spaces.")

(defvar lozenge-file-extension-regex "\\(\\.loz\\)\\..+$"
  "Run against function `buffer-file-name' to see if this is a lozenge file.")

(defvar lozenge-find-file-after-export t
  "Should the exported file be opened after `lozenge-export'?")

(defun lozenge-in-comment-p ()
  "Check if `point' is in a comment."
  (if (assoc major-mode lozenge-in-comment-check-list)
      (eval (cdr (assoc major-mode lozenge-in-comment-check-list)))
    (nth 4 (syntax-ppss))))

(defun lozenge-real-sexp (sexp)
  "Check if car of SEXP has a variant including `lozenge-symbol-prefix'.
SEXP can be either an sexp or a symbol.
Checks in order if one of the following symbols exists:

1. `projectile-project-name'/`lozenge-symbol-prefix'(car sexp)
    Example: my-project/loz/foobar
2. `lozenge-symbol-prefix'`major-mode'/(car  sexp)
    Example: loz/org-mode/foobar
3. `lozenge-symbol-prefix'(car sexp)
    Example: loz/foobar

If it does, then use that symbol instead."
  (cond
   ((symbolp sexp)
    (or (and (require 'projectile nil t)
             (projectile-project-root)
             (intern-soft (concat (projectile-project-name) "/"
                                  lozenge-symbol-prefix (symbol-name sexp))))
        (intern-soft (concat lozenge-symbol-prefix
                             (symbol-name major-mode) "/" (symbol-name sexp)))
        (intern-soft (concat lozenge-symbol-prefix (symbol-name sexp)))
        sexp))
   ((listp sexp)
    (cons (lozenge-real-sexp (car sexp)) (cdr sexp)))))

(defun lozenge-list-to-html (list format-args)
  "Turn LIST to html using `xmlgen'.
Send the result to `format' using FORMAT-ARGS."
  (apply #'format (xmlgen list) format-args))

(defun lozenge-list-to-org-html (list format-args)
  "Send LIST to `xmlgen' and wrap the result in @@html:<LIST>@@.
If LIST has any strings with %s in it, they will be replaced by FORMAT-ARGS.
The FORMAT-ARGS are outside of the @@html:@@ wrapping."
  (if format-args
      (apply #'format
             (mapconcat
              (lambda (x) (format "@@html:%s@@" x))
              (split-string (xmlgen list) "%s")
              "%s")
             format-args)
    (format "@@html:%s@@" (xmlgen list))))

(defun lozenge--regex ()
  "Get regex matching `lozenge-lozenge-char' or `lozenge-half-lozenge-char'."
  (format "[%s]" (string lozenge-lozenge-char lozenge-half-lozenge-char)))

(defun lozenge-at-point (&optional print-message)
  "Get lozenge sexp at point. Maybe PRINT-MESSAGE."
  (interactive "p")
  (let ((start (point)))
    (if (not (looking-at-p (lozenge--regex)))
        (message "No Lozenge at point")
      (save-excursion
        (forward-char 1)
        (insert " ")                    ; So sexp won't start with ◊
        (let* ((sexp (sexp-at-point))
               (text-arg (progn
                           (delete-char -1) ; Delete the space
                           (forward-sexp)
                           (when (looking-at-p "{")
                             (let ((arg (-> (thing-at-point 'list t)
                                            (string-trim-left "{")
                                            (string-trim-right "}"))))
                               (forward-sexp)
                               arg))))
               (format-args (let (strings)
                              (while (looking-at-p "\n?\\[[^\\[]")
                                (when (looking-at-p "\n")
                                  (delete-char 1))
                                (let ((arg (-> (thing-at-point 'list t)
                                               (string-trim-left "\\[")
                                               (string-trim-right "\\]"))))
                                  (forward-sexp)
                                  (push arg strings)))
                              (reverse strings)))
               (result (cond
                        ((and (listp sexp) (keywordp (car sexp)))
                         (let ((classes
                                (-> (substring (symbol-name (car sexp)) 1)
                                    (split-string "\\.")
                                    (string-join " ")))
                               (text (concat (mapconcat 'symbol-name (cdr sexp) " "))))
                           (setq format-args (append format-args
                                                     (list text)
                                                     (list text-arg)))
                           (if text-arg
                               `(div :class ,classes "%s %s")
                             `(span :class ,classes "%s"))))
                        (text-arg
                         (if (listp sexp)
                             (let ((head (lozenge-real-sexp (car sexp))))
                               (if (eq head 'quote)
                                   (progn
                                     (setq format-args
                                           (append format-args (list text-arg)))
                                     (setf (cadr sexp) (append (cadr sexp) (list "%s")))
                                     (eval sexp))
                                 (apply head (append (list text-arg) (cdr sexp)))))
                           (funcall (lozenge-real-sexp sexp) text-arg)))
                        ((stringp sexp)
                         sexp)
                        (t (eval (lozenge-real-sexp sexp))))))
          (when result
            (let ((info (list start (point) result format-args)))
              (when print-message
                (prin1 info))
              info)))))))

(defun lozenge-eval-replace (&optional backend)
  "Evaluate the lozenge sexp at point and replace it with the result.
BACKEND is an `org-export' symbol; html if not specified."
  (interactive)
  (setq backend (or backend 'html))
  (if-let* ((loz-form (lozenge-at-point))
            (result (nth 2 loz-form)))
      (let ((format-args (nth 3 loz-form)))
        (delete-region (nth 0 loz-form) (nth 1 loz-form))
        (save-excursion
          (insert
           (cond ((stringp result)
                  (apply 'format result format-args))
                 ((numberp result)
                  (number-to-string result))
                 ((listp result)
                  (let ((mm-symbol major-mode))
                    (when (eq mm-symbol 'org-mode)
                      (setq mm-symbol (intern (concat "org-mode/" (symbol-name backend)))))
                    (if (assoc mm-symbol lozenge-list-processing-functions)
                        (funcall (cdr (assoc mm-symbol lozenge-list-processing-functions))
                                 result format-args)
                      (mapconcat (lambda (x) (format "%s" x))
                                 result " "))))
                 (t "")))))
    (error "Lozenge sexp returned nil")))

(defun lozenge--remove-backslash (char)
  "The lozenge CHAR can be escaped with a backslash.
This function removes those backslashes."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward (concat "[\\\]" (string char)) nil t)
     (backward-char 2)
     (delete-char 1))))

(defun lozenge--eval-replace-buffer (char &optional backend)
  "Run `lozenge-eval-replace' on all CHAR lozenge sexps in current buffer.
Then remove backslash escaped CHAR with `lozenge--remove-backslash'.
BACKEND is used by `org-export'."
  (goto-char (point-min))
  (while (re-search-forward (concat "[^\\\]" (char-to-string char)) nil t)
    (unless (lozenge-in-comment-p)
      (backward-char 1)
      (lozenge-eval-replace backend)))
  (lozenge--remove-backslash char))

(defun lozenge-replace-all-lozenges (&optional backend)
  "Replace all `lozenge-lozenge-char' in current buffer.
BACKEND is used by `org-export'."
  (org-with-wide-buffer (lozenge--eval-replace-buffer lozenge-lozenge-char backend)))

(defun lozenge-replace-all-half-lozenges (&optional backend)
  "Replace all `lozenge-half-lozenge-char' in current buffer.
BACKEND is used by `org-export'."
  (org-with-wide-buffer (lozenge--eval-replace-buffer lozenge-half-lozenge-char backend)))

(defun lozenge-insert-lozenge ()
  "Insert the lozenge char."
  (interactive)
  (insert lozenge-lozenge-char))

(defun lozenge-insert-half-lozenge ()
  "Insert the half-lozenge char."
  (interactive)
  (insert lozenge-half-lozenge-char))

(defun lozenge-forward (&optional N)
  "Move point N lozenges forward (backward if N is negative).
Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 lozenge forward."
  (interactive "p")
  (if (< N 0)
      (lozenge-backward (- N))
    (forward-char 1)
    (condition-case err
        (re-search-forward (lozenge--regex) nil nil N)
      (error (progn (backward-char 1)
                    (error (error-message-string err)))))
    (backward-char 1)))

(defun lozenge-backward (&optional N)
  "Move point N lozenges backward (forward if N is negative).
Interactively, N is the numeric prefix argument.
If N is omitted or nil, move point 1 lozenge backward."
  (interactive "p")
  (if (< N 0)
      (lozenge-forward (- N))
    (re-search-backward (lozenge--regex) nil nil N)))

(defun lozenge-export (file)
  "Eval and replace all lozenges in FILE and export it to a new one.
First all half-lozenges are replaced, then all regular lozenges.
The FILE name must match `lozenge-file-extension-regex', where
the first match group will be removed in the exported file.
By default a file named `foo.loz.bar' will be exported as `foo.bar'."
  (interactive (list (buffer-file-name)))
  (let ((exported-file (replace-regexp-in-string
                        lozenge-file-extension-regex "" file t nil 1))
        (mode major-mode))
    (if (equal file exported-file)
        (user-error "File name doesn't match lozenge-file-extension-regex: %s"
                    lozenge-file-extension-regex)
      (with-temp-file exported-file
        (delay-mode-hooks
          (insert-file-contents file)
          (funcall mode)
          (lozenge-replace-all-half-lozenges)
          (lozenge-replace-all-lozenges)))
      (when lozenge-find-file-after-export
        (find-file-other-window exported-file))
      (message "Wrote file %s" exported-file))))

(defun lozenge-org-export-enable ()
  "`lozenge' will be used when exporting from  `org-mode'."
  (interactive)
  (add-hook 'org-export-before-parsing-hook #'lozenge-replace-all-lozenges)
  (add-hook 'org-export-before-processing-hook #'lozenge-replace-all-half-lozenges)
  (message "Enabled lozenge export"))

(defun lozenge-org-export-disable ()
  "Disable `lozenge' when exporting from `org-mode'."
  (interactive)
  (remove-hook 'org-export-before-parsing-hook #'lozenge-replace-all-lozenges)
  (remove-hook 'org-export-before-processing-hook #'lozenge-replace-all-half-lozenges)
  (message "Disabled lozenge export"))

(with-eval-after-load 'markdown-mode
  (defun lozenge-markdown-export-enable ()
    "`lozenge' will be used when exporting from  `markdown-mode'."
    (interactive)
    (add-hook 'markdown-before-export-hook #'lozenge-replace-all-half-lozenges)
    (add-hook 'markdown-before-export-hook #'lozenge-replace-all-lozenges)
    (message "Enabled lozenge export"))

  (defun lozenge-markdown-export-disable ()
    "Disable `lozenge' when exporting from `markdown-mode'."
    (interactive)
    (remove-hook 'markdown-before-export-hook #'lozenge-replace-all-half-lozenges)
    (remove-hook 'markdown-before-export-hook #'lozenge-replace-all-lozenges)
    (message "Disabled lozenge export")))

(provide 'lozenge)
;;; lozenge.el ends here
