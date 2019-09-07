;;; org-lozenge.el --- Pollen inspired Lozenge-syntax for org-mode   -*- lexical-binding: t; -*-

;; Copyright (C) 2019-- Erik Sjöstrand
;; MIT License

;; Author: Erik Sjöstrand
;; URL: http://github.com/Kungsgeten/org-lozenge
;; Version: 1.00
;; Package-Requires: ((emacs "25") (org "9.2") (xmlgen "0.5") (dash "2.16.0"))

;;; Commentary:
;; Pollen inspired Lozenge-syntax for org-mode.
;; See examples etc in the repo.

;;; Code:
(require 'xmlgen)
(require 'dash)

(defvar org-lozenge-symbol-prefix "loz/"
  "`org-lozenge' will check for variables and functions with this prefix.")

(defvar org-lozenge-before-parsing-char ?◊
  "The lozenge char.")

(defvar org-lozenge-before-processing-char ?⟠
  "The half-lozenge char.")

(defun org-lozenge-real-sexp (sexp)
  "Check if car of SEXP has a variant starting with `org-lozenge-symbol-prefix'.
If it does, then use that one instead.
SEXP can be either an sexp or a symbol."
  (cond
   ((symbolp sexp)
    (or (intern-soft (concat org-lozenge-symbol-prefix (symbol-name sexp)))
        sexp))
   ((listp sexp)
    (cons (org-lozenge-real-sexp (car sexp)) (cdr sexp)))))

(defun org-lozenge-list-to-org-html (list format-args)
  "Send LIST to `xmlgen' and wrap the result in @@html:<LIST>@@.
If LIST has any strings with %s in it, they will be replaced by FORMAT-ARGS.
The FORMAT-ARGS are outside of the @@html:@@ wrapping."
  (if format-args
      (apply 'format
             (mapconcat
              (lambda (x) (format "@@html:%s@@" x))
              (split-string (xmlgen list) "%s")
              "%s")
             format-args)
    (format "@@html:%s@@" (xmlgen list))))

(defun org-lozenge-at-point (&optional print-message)
  "Get lozenge sexp at point. Maybe PRINT-MESSAGE."
  (interactive "p")
  (let ((start (point)))
    (if (not (looking-at-p (format "[%s]" (string org-lozenge-before-parsing-char
                                                  org-lozenge-before-processing-char))))
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
                             (let ((head (org-lozenge-real-sexp (car sexp))))
                               (if (eq head 'quote)
                                   (progn
                                     (setq format-args
                                           (append format-args (list text-arg)))
                                     (setf (cadr sexp) (append (cadr sexp) (list "%s")))
                                     (eval sexp))
                                 (apply head (append (list text-arg) (cdr sexp)))))
                           (funcall (org-lozenge-real-sexp sexp) text-arg)))
                        ((stringp sexp)
                         sexp)
                        (t (eval (org-lozenge-real-sexp sexp))))))
          (when result
            (let ((info (list start (point) result format-args)))
              (when print-message
                (prin1 info))
              info)))))))

(defun org-lozenge-eval-replace (&optional backend)
  "Evaluate the lozenge sexp at point and replace it with the result.
BACKEND is an `org-export' symbol; html if not specified."
  (interactive)
  (if-let* ((lozenge-form (org-lozenge-at-point))
            (result (nth 2 lozenge-form)))
      (let ((format-args (nth 3 lozenge-form)))
        (delete-region (nth 0 lozenge-form) (nth 1 lozenge-form))
        (save-excursion
          (insert
           (cond ((stringp result)
                  (apply 'format result format-args))
                 ((numberp result)
                  (number-to-string result))
                 ((listp result)
                  (if (or (not backend) (eq backend 'html))
                      (org-lozenge-list-to-org-html result format-args)
                    (mapconcat (lambda (x) (format "%s" x))
                               result " ")))
                 (t "")))))
    (error "Lozenge sexp returned nil")))

(defun org-lozenge--remove-backslash (char)
  "The lozenge CHAR can be escaped with a backslash.
This function removes those backslashes."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward (concat "[\\\]" (string char)) nil t)
     (backward-char 2)
     (delete-char 1))))

(defun org-lozenge-before-parsing-replace (&optional backend)
  "Replace all lozenge sexps in current buffer.
BACKEND is used by `org-export'."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward
           (concat "[^\\\]" (string org-lozenge-before-parsing-char)) nil t)
     (unless (or (org-in-commented-heading-p)
                 (org-at-comment-p))
       (backward-char 1)
       (org-lozenge-eval-replace backend))))
  (org-lozenge--remove-backslash org-lozenge-before-parsing-char)
  (org-lozenge--remove-backslash org-lozenge-before-processing-char))

(defun org-lozenge-before-processing-replace (&optional backend)
  "Replace all half-lozenge sexps in current buffer.
BACKEND is used by `org-export'."
  (org-with-wide-buffer
   (goto-char (point-min))
   (while (re-search-forward
           (concat "[^\\\]" (string org-lozenge-before-processing-char)) nil t)
     (unless (or (org-in-commented-heading-p)
                 (org-at-comment-p))
       (backward-char 1)
       (org-lozenge-eval-replace backend)))))

(defun org-lozenge-insert-lozenge ()
  "Insert the lozenge char."
  (interactive)
  (insert org-lozenge-before-parsing-char))

(defun org-lozenge-insert-half-lozenge ()
  "Insert the half-lozenge char."
  (interactive)
  (insert org-lozenge-before-processing-char))

(defun org-lozenge-enable ()
  "`org-lozenge' will be used when exporting from  `org-mode'."
  (interactive)
  (add-hook 'org-export-before-parsing-hook #'org-lozenge-before-parsing-replace)
  (add-hook 'org-export-before-processing-hook #'org-lozenge-before-processing-replace)
  (message "Enabled org-lozenge"))

(defun org-lozenge-disable ()
  "Disable `org-lozenge' when exporting from `org-mode'."
  (interactive)
  (remove-hook 'org-export-before-parsing-hook #'org-lozenge-before-parsing-replace)
  (remove-hook 'org-export-before-processing-hook #'org-lozenge-before-processing-replace)
  (message "Disabled org-lozenge"))

(provide 'org-lozenge)
;;; org-lozenge.el ends here
