;;; 02_frame.el

;;; Code:

;; frame title
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;;; window setting
;; hide toolbar, scroll-bar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;; mode-line setting
;; show line number
(line-number-mode t)
;; show column number
(column-number-mode t)
;; show battery force
(display-battery-mode t)

;; display line and char count
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines, %d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

