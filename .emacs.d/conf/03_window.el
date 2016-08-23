;;; 03_window.el

;;; Code:

;; show parent
(setq show-paren-delay 0)
(show-paren-mode t)

;; truncate
(setq-default truncate-lines t)

;; enlighten editing line
(global-hl-line-mode t)

;;; cursor
;; cursor not blink
(blink-cursor-mode 0)

;; hi-light region
(transient-mark-mode)
(setq highlight-nonselected-windows t)

;; temporary hi-light after yank region
(when window-system
  (defadvice yank (after ys:yank-highlight activate)
    (let ((ol (make-overlay (mark t) (point))))
      (overlay-put ol 'face 'highlight)
      (sit-for 1.0)
      (delete-overlay ol)))
  (defadvice yank-pop (after ys:yank-pop-highlight activate)
    (when (eq last-command 'yank)
      (let ((ol (make-overlay (mark t) (point))))
        (overlay-put ol 'face 'highlight)
        (sit-for 1.0)
        (delete-overlay ol)))))

;; open with drag file
(define-key global-map [ns-drag-file] 'ns-find-file)

;; save buffer condition
(desktop-save-mode t)
