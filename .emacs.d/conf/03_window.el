;;; 03_window.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:16:30
;; Last-Updated: <2015/12/29 15:20:06>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; 

;;; Code:

;; insert "\", instead "¥"
(define-key global-map [?¥] [?\\])

;; tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; delete region, when yank
(delete-selection-mode t)

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

;; display non cursor in non active-window
(setq-default cursor-in-non-selected-windows t)

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
