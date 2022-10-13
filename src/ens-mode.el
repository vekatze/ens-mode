;;; ens-mode.el --- ens mode -*- lexical-binding: t; -*-
;; Author: vekatze <vekatze@icloud.com>
;;; Commentary:
;; A major mode for ens configuration language.
;;; Code:

(define-derived-mode ens-mode prog-mode "ens"
  "A major mode for ens."
  (add-to-list 'auto-mode-alist '("\\.ens\\'" . ens-mode))
  (set (make-local-variable 'comment-start) "//")
  (set (make-local-variable 'comment-end) "")
  (set (make-local-variable 'comment-padding) 1)
  (set (make-local-variable 'comment-use-syntax) t)
  (set-syntax-table
   (let ((syntax-table (make-syntax-table)))
     (modify-syntax-entry ?/ "w 12b" syntax-table)
     (modify-syntax-entry ?\n "> b" syntax-table)
     (modify-syntax-entry ?# "w" syntax-table)
     (modify-syntax-entry ?: "w" syntax-table)
     syntax-table))
  (setq font-lock-defaults
        `(,`((,(regexp-opt '("true" "false") 'symbols)
              . font-lock-builtin-face)
             ("\\([^ \n]+\\)[ \t]+="
              . (1 font-lock-keyword-face))
             ))))

(autoload 'ens-mode "ens-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.ens$" . ens-mode))

(provide 'ens-mode)

;;; ens-mode.el ends here
