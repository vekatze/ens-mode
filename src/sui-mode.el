;;; sui-mode.el --- sui mode -*- lexical-binding: t; -*-

;; Author: veka41 <veka41@protonmail.ch>

;;; Commentary:

;; A major mode for sui configuration language.

;;; Code:

(define-derived-mode sui-mode prog-mode "sui"
  "A major mode for sui."
  (add-to-list 'auto-mode-alist '("\\.sui\\'" . sui-mode))
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

(provide 'sui-mode)

;;; sui-mode.el ends here
