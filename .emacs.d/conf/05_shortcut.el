;;; 05_shortcut.el

;;; Code:
(bind-keys ("C-c a"   . align)
           ("C-c S-a" . align-regexp)
           ("C-c d"   . delete-trailing-whitespace)
           ("C-c b"   . battery)
           ("C-x e"   . eval-last-sexp)
           ("M-r"     . query-replace)
           ("M-c"     . c-mode))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-x r f r : save frame configuration
;; C-x r j r : restore frame configuration

;; Cmd-Ctl-d ; open apple's default dicitonaly
