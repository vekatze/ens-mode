;;; enti-mode.el --- enti mode -*- lexical-binding: t; -*-

;; Author: veka41 <veka41@protonmail.ch>

;;; Commentary:

;; A major mode for enti configuration language.

;;; Code:

(define-derived-mode enti-mode prog-mode "enti"
  "A major mode for Enti programming language."
  (add-to-list 'auto-mode-alist '("\\.enti\\'" . enti-mode))
  (set (make-local-variable 'comment-start) "--")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?- "w 12b" syntax-table)
     (modify-syntax-entry ?\n "> b" syntax-table)
     (modify-syntax-entry ?# "w" syntax-table)
     (modify-syntax-entry ?: "w" syntax-table)
     syntax-table))
  (setq font-lock-defaults
        `(,`((,(regexp-opt '("true" "false") 'symbols)
              . font-lock-builtin-face)
             ("\\([^ \n]+\\)[ \t]+="
              . (1 font-lock-keyword-face))))))

(provide 'enti-mode)

;;; enti-mode.el ends here
