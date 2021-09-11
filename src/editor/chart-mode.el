;;; chart-mode.el --- chart mode -*- lexical-binding: t; -*-

;; Author: veka41 <veka41@protonmail.ch>

;;; Commentary:

;; A major mode for chart configuration language.

;;; Code:

(define-derived-mode chart-mode prog-mode "chart"
  "A major mode for Chart programming language."
  (add-to-list 'auto-mode-alist '("\\.chart\\'" . chart-mode))
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?- "w 12b" syntax-table)
     (modify-syntax-entry ?\n "> b" syntax-table)
     (modify-syntax-entry ?# "w" syntax-table)
     syntax-table))
  (setq font-lock-defaults
        `(,`(("\\(#[^ \n]+\\)"
              . (1 font-lock-constant-face))
             ("\\([^ \n]+\\)[ \t]+="
              . (1 font-lock-keyword-face))))))

(provide 'chart-mode)

;;; chart-mode.el ends here
