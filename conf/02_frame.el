;;; 02_frame.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:20:26>
;; Last-Updated: <2015/12/05 01:44:30>
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

