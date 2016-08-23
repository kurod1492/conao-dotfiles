;;; swap.el --- Swap mode -*-emacs-lisp-*-

;; Copyright (C) 2003 Naoki Nakamura

;; Author:  Naoki Nakamura <naoki.y.nakamura@nifty.com>
;; Keywords: swap, region, rectangle

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;;; How to install:

;; To install, put this in your .emacs file:
;;
;;  (autoload 'swap-region "swap" "swap region." t nil)
;;  (autoload 'swap-rectangle "swap" "swap rectangle." t nil)
;;
;; And bind it to any key, as you like.
;;

;;; ChangeLog:

;; Swap ver. 1.0
;;

;;; Code:

(defface swap-mark-overlay-face
  '((t (:foreground "pink" :background "DeepSkyBlue4")))
  "Name of face (quoted symbol) to use for swap highlighting."
  :group 'face
  :group 'swap)

(defvar swap-point  nil)
(defvar swap-window nil)
(defvar swap-type nil)
(defvar swap-region-from-beg nil)
(defvar swap-region-from-end nil)
(defvar swap-rectangle-from-beg nil)
(defvar swap-rectangle-from-end nil)
(make-variable-buffer-local 'swap-point)
(make-variable-buffer-local 'swap-window)
(make-variable-buffer-local 'swap-type)
(make-variable-buffer-local 'swap-region-from-beg)
(make-variable-buffer-local 'swap-region-from-end)
(make-variable-buffer-local 'swap-rectangle-from-beg)
(make-variable-buffer-local 'swap-rectangle-from-end)

(defvar swap-major-mode nil)
(make-variable-buffer-local 'swap-major-mode)
(defvar swap-mode-name  nil)
(make-variable-buffer-local 'swap-mode-name)
(defvar swap-local-map  nil)
(make-variable-buffer-local 'swap-local-map)
(defvar swap-buffer-read-only nil)
(make-variable-buffer-local 'swap-buffer-read-only)


;;; key map

(defvar swap-mode-map nil)
(if swap-mode-map
    nil
  (let ((map (make-keymap))
        (key ?!))
    ;; destroy default keymap
    (while (<= key ?~)
      (define-key map
        (char-to-string key) 'swap-undefined)
      (setq key (1+ key)))
    ;; quit and exit.
    (define-key map "q" 'swap-exit)
    (substitute-key-definition 'keyboard-quit 'swap-quit map global-map)
    (define-key map "\r" 'swap-do)
    ;; help and prefix
    (define-key map "?"  'describe-mode)
    (setq swap-mode-map map)))


;;; Commands

;;;###autoload
(defun swap-mode ()
  "Major mode for swapping text but not editing it.

\\{swap-mode-map}

Entry to this mode calls the value of `swap-mode-hook' if that value
is non-nil."
  (unless (eq major-mode 'swap-mode)
    (if buffer-read-only
        ;; Swap mode can not work in read only buffer.
        (error "This buffer is read only.  Swap can not work")
      (setq swap-window (current-window-configuration)
            swap-point  (point))
      ;; Main body
      (swap)
      (run-hooks 'swap-mode-hook))))

(defun swap ()
  "Set current buffer swap-mode."
  (unless (eq major-mode 'swap-mode)
    (setq swap-major-mode major-mode
          swap-mode-name  mode-name
          swap-local-map  (current-local-map))
    (setq major-mode 'swap-mode
          mode-name  "Swap")
    (use-local-map   swap-mode-map)
    (if buffer-read-only
        (setq swap-buffer-read-only t)
      (setq buffer-read-only t))
    (force-mode-line-update)))

(defun swap-undefined ()
  (interactive)
  (ding)
  (message "Undefined."))


;;; Quit and Exit.

(defun swap-exit ()
  "Exit swap-mode without point move.
If called uninteractively, return window configuretion.
\\[swap-quit] move point back ot where swap start."
  (interactive)
  (setq major-mode swap-major-mode
        mode-name  swap-mode-name)
  (use-local-map swap-local-map)
  (setq swap-major-mode nil
        swap-mode-name  nil
        swap-local-map  nil)
  (if swap-buffer-read-only
      (setq swap-buffer-read-only nil)
    (setq buffer-read-only nil))
  (unless (interactive-p)
    (set-window-configuration swap-window))
  (force-mode-line-update)
  (swap-clear))

(defun swap-quit ()
  "Exit swap-mode and back to where swap start.
\\[swap-exit] do not move point and exit swap."
  (interactive)
  (swap-exit)
  (ding)
  (goto-char swap-point)
  (recenter))

(defun swap-region()
  (interactive)
  (swap-mode)
  (setq swap-type 'region))

(defun swap-rectangle()
  (interactive)
  (swap-mode)
  (setq swap-type 'rectangle))

(defun swap-clear()
  ;; clean up overlays
  (mapcar (lambda(x)
            (when (overlay-get x 'swap)
              (delete-overlay x)))
          (overlays-in (point-min) (point-max)))
  ;; reset
  (setq swap-type               nil
        swap-region-from-beg    nil
        swap-region-from-end    nil
        swap-rectangle-from-beg nil
        swap-rectangle-from-end nil))

(defun swap-do (beg end)
  (interactive "r")
  (cond ((eq swap-type 'region)
         (cond (swap-region-from-beg
                (swap-region-to beg end))
               (t
                (swap-region-from beg end)
                (message "Then, mark target region and hit Enter key."))))
        ((eq swap-type 'rectangle)
         (cond (swap-rectangle-from-beg
                (swap-rectangle-to beg end))
               (t
                (swap-rectangle-from beg end)
                (message "Then, mark target rectangle and hit Enter key."))))))

(defun swap-region-from (beg end)
  (setq swap-region-from-beg (copy-marker beg)
        swap-region-from-end (copy-marker end))
  (swap-mark-region beg end)
  (deactivate-mark))

(defun swap-region-to (beg end)
  (let* ((from-beg swap-region-from-beg)
         (from-end swap-region-from-end)
         (this-beg (copy-marker beg))
         (from-region (buffer-substring (marker-position from-beg)
                                        (marker-position from-end)))
         (this-region (buffer-substring beg end)))
    (swap-exit)
    (delete-region beg end)
    (goto-char (marker-position this-beg))
    (insert from-region)
    (delete-region (marker-position from-beg)
                   (marker-position from-end))
    (goto-char (marker-position from-beg))
    (insert this-region)
    (message "Region swapped!!")))

(defun swap-rectangle-from (beg end)
  (setq swap-rectangle-from-beg (copy-marker beg)
        swap-rectangle-from-end (copy-marker end))
  (swap-mark-rectangle beg end)
  (deactivate-mark))

(defun swap-rectangle-to (beg end)
  (let* ((from-beg swap-rectangle-from-beg)
         (from-end swap-rectangle-from-end)
         (this-beg (copy-marker beg))
         (from-rect (extract-rectangle
                     (marker-position from-beg)
                     (marker-position from-end)))
         (rect (extract-rectangle beg end)))
    (swap-exit)
    (delete-rectangle beg end)
    (goto-char (marker-position this-beg))
    (insert-rectangle from-rect)
    (delete-rectangle (marker-position from-beg)
                      (marker-position from-end))
    (goto-char (marker-position from-beg))
    (insert-rectangle rect)
    (message "Rectangle swapped!!")))

(defun swap-mark-region (start end)
  "Highlight region between START and END."
  (if (and (interactive-p) transient-mark-mode mark-active)
      (deactivate-mark))
  (let ((ov (make-overlay start end)))
    (overlay-put ov 'face 'swap-mark-overlay-face)
    (overlay-put ov 'priority 100)
    (overlay-put ov 'swap t)))

(defun swap-mark-rectangle (start end)
  "Highlight contents of rectangle between mark and point."
  (if (and (interactive-p) transient-mark-mode mark-active)
      (deactivate-mark))
  (operate-on-rectangle 'swap-mark-rectangle-line start end nil))

(defun swap-mark-rectangle-line (startpos begextra endextra)
  (swap-mark-region startpos (point)))

;; By default operate-on-rectangle is not available
(autoload 'operate-on-rectangle "rect"
  "Call FUNCTION for each line of rectangle.")


;;;
(provide 'swap)

;;; End of swap.el