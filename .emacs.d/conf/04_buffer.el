;;; 04_buffer.el

;;; Code:

;; insert "\", instead "¥"
(define-key global-map [?¥] [?\\])

;; tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; delete region, when yank
(delete-selection-mode t)

;; comment style
(setq comment-style 'multi-line)
