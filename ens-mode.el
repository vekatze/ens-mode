;;; ens-mode.el --- ens mode -*- lexical-binding: t; -*-
;; Author: vekatze <vekatze@icloud.com>
;;; Commentary:
;; A major mode for ens configuration language.
;;; Code:


(defvar ens-mode-indent-offset 2)

(defun ens--get-offset-from-eol ()
  (- (line-end-position) (point)))

(defun ens-mode-indent-line ()
  (interactive)
  (let ((original-offset-from-eol (ens--get-offset-from-eol)))
    (indent-line-to (ens--calculate-indentation))
    (when (< original-offset-from-eol (ens--get-offset-from-eol))
      (goto-char (- (line-end-position) original-offset-from-eol)))))

(defun ens--calculate-indentation ()
  (if (or (ens--in-string-p (point))
          (ens--in-string-p (line-beginning-position)))
      (ens--get-indentation-of (point)) ;; leave strings as they are
    (let* ((child-line-number (line-number-at-pos (point)))
           (child-indentation (ens--get-indentation-of child-line-number))
           (parent-pos-or-none (save-excursion
                                 (ens--goto-indent-base-position)
                                 (ens--get-parent 0)))
           (parent-indentation (ens--get-indentation-of parent-pos-or-none)))
      (+ parent-indentation ens-mode-indent-offset))))

(defun ens--get-indentation-of (pos)
  (if pos
      (save-excursion
        (ens--goto-line (line-number-at-pos pos))
        (ens--get-first-char-column))
    (* -1 ens-mode-indent-offset)))

(defun ens--goto-indent-base-position ()
  (goto-char (line-end-position))
  (goto-char (ens--find-shallowest-point 0 0 (point))))

(defun ens--goto-line (line-number)
  (goto-char (point-min))
  (forward-line (- line-number 1)))

(defun ens--goto-first-char-column ()
  (goto-char (line-beginning-position))
  (skip-chars-forward " ")
  (current-column))

(defun ens--get-first-char-column ()
  (save-excursion (ens--goto-first-char-column)))

(defun ens--in-string-p (pos)
  (nth 3 (syntax-ppss pos)))

(defun ens--skip-comment (start)
  (interactive)
  (when (re-search-backward "//" (line-beginning-position) t)
    (let ((pos (match-beginning 0)))
      (if (ens--in-string-p pos)
          (goto-char start)
        (ens--skip-comment pos)))))

(defun ens--find-shallowest-point (eval-value max-eval-value shallowest-pos)
  "Backtrack from the end of a line and find (one of) the shallowest point of the line.

The shallowness of a point is evaluated by `eval-value'; This value is incremented when
an open paren is found, and decremented when a closing paren is found."
  (let ((char (preceding-char)))
    (cond
     ((= char 0)
      shallowest-pos)
     ((= char ?\n)
      shallowest-pos)
     ((= char ? )
      (goto-char (- (point) 1))
      (ens--find-shallowest-point eval-value max-eval-value shallowest-pos))
     ;; found an opening paren
     ((ens--opening-paren-p char)
      (goto-char (- (point) 1))
      (ens--backtrack-opening-paren eval-value max-eval-value shallowest-pos))
     ;; found a closing paren
     ((ens--closing-paren-p char)
      (goto-char (- (point) 1))
      (ens--backtrack-closing-paren eval-value max-eval-value shallowest-pos))
     (t
      (let ((token (ens--get-token (point))))
        (cond
         ((string= token "")
          shallowest-pos)
         (t
          (ens--find-shallowest-point eval-value max-eval-value shallowest-pos))))))))

(defun ens--backtrack-opening-paren (eval-value max-eval-value shallowest-pos)
  (let ((next-eval-value (+ eval-value 1)))
    (if (> next-eval-value max-eval-value)
        (ens--find-shallowest-point next-eval-value next-eval-value (point))
      (ens--find-shallowest-point next-eval-value max-eval-value shallowest-pos))))

(defun ens--backtrack-closing-paren (eval-value max-eval-value shallowest-pos)
  (ens--find-shallowest-point (- eval-value 1) max-eval-value shallowest-pos))

(defun ens--get-parent (nest-level)
  "Find the nearest encloser of current point by backtracking. Returns nil if the encloser is the file itself.

The `nest-level' is just to handle nested (let .. in).
This function must be called from outside a string."
  (let ((char (preceding-char)))
    (cond
     ((= char 0)
      nil)
     ((ens--skip-p char)
      (goto-char (- (point) 1))
      (ens--get-parent nest-level))
     ((ens--newline-p char)
      (goto-char (- (point) 1))
      (ens--skip-comment (point))
      (ens--get-parent nest-level))
     ((ens--opening-paren-p char)
      (point))
     ((ens--closing-paren-p char)
      (goto-char (scan-sexps (point) -1)) ;; skip a paren-pair
      (ens--get-parent nest-level))
     ((ens--double-quote-p char)
      (goto-char (scan-sexps (point) -1)) ;; skip a string
      (ens--get-parent nest-level))
     (t
      (let ((token (ens--get-token (point))))
        (ens--get-parent nest-level))))))

(defun ens--get-token (initial-position)
  (let ((char (preceding-char)))
    (cond
     ((eq (point) (line-beginning-position))
      (buffer-substring-no-properties (point) initial-position))
     ((ens--non-token-p char)
      (buffer-substring-no-properties (point) initial-position))
     (t
      (goto-char (- (point) 1))
      (ens--get-token initial-position)))))

(defun ens--make-hash-table (chars)
  (let ((table (make-hash-table :test 'equal)))
    (dolist (char chars)
      (puthash char t table))
    table))

(defconst ens--opening-paren-char-set
  (ens--make-hash-table (list ?{ ?\( ?\[)))
(defconst ens--closing-paren-char-set
  (ens--make-hash-table (list ?} ?\) ?\])))
(defconst ens--skip-char-set
  (ens--make-hash-table (list ?\s ?, ?: ?\; ?&)))
(defconst ens--newline-char-set
  (ens--make-hash-table (list ?\n)))
(defconst ens--double-quote-char-set
  (ens--make-hash-table (list ?\")))
(defconst ens--non-token-char-set
  (ens--make-hash-table (list ?{ ?} ?\( ?\) ?\[ ?\] ?< ?> ?\s ?\n ?\;)))
(defun ens--opening-paren-p (char)
  (gethash char ens--opening-paren-char-set))
(defun ens--closing-paren-p (char)
  (gethash char ens--closing-paren-char-set))
(defun ens--skip-p (char)
  (gethash char ens--skip-char-set))
(defun ens--newline-p (char)
  (gethash char ens--newline-char-set))
(defun ens--double-quote-p (char)
  (gethash char ens--double-quote-char-set))
(defun ens--non-token-p (char)
  (gethash char ens--non-token-char-set))

;;
;; utils
;;

(defun ens--line-empty-p ()
  (string-match-p "\\`\\s-*$" (thing-at-point 'line)))

;;;###autoload
(define-derived-mode ens-mode prog-mode "ens"
  "A major mode for ens."
  (add-to-list 'auto-mode-alist '("\\.ens\\'" . ens-mode))
  (setq-local comment-start "//")
  (setq-local comment-end "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (setq-local indent-line-function 'ens-mode-indent-line)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?/ "_ 12" syntax-table)
     (modify-syntax-entry ?\n ">" syntax-table)
     (modify-syntax-entry ?# "w" syntax-table)
     (modify-syntax-entry ?: "w" syntax-table)
     syntax-table))
  (setq font-lock-defaults
        `(,`((,(regexp-opt '("true" "false") 'symbols)
              . font-lock-builtin-face)
             (,(regexp-opt '("-") 'symbols)
              . font-lock-builtin-face)
             ("\\([^ \n]+\\)[ \t]+"
              . (1 font-lock-keyword-face))
             (,(regexp-opt '("target" "dependency" "foreign" "source" "extra-content" "archive" "build" "structure"))
              . font-lock-keyword-face)
             ))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.ens$" . ens-mode))

(provide 'ens-mode)

;;; ens-mode.el ends here
