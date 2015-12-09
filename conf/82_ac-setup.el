;;; 82_ac-setup.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/29 16:06:45>
;; Last-Updated: <2015/11/30 17:20:46>
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
(defun my-ac-common-setup ()
  (setq ac-sources '(ac-source-filename
                     ac-source-abbrev
                     ac-source-gtags
                     ac-source-yasnippet
                     ac-source-dictionary
                     ac-source-words-in-same-mode-buffers)))
(defun my-ac-emacs-lisp-setup ()
  (append ac-sources '(ac-source-functions
                       ac-source-variables
                       ac-source-symbols
                       ac-source-features)))
(defun my-ac-cc-mode-setup ()
  (append ac-sources '(ac-source-semantic)))
(defun my-css-mode-setup ()
  (append ac-sources '(ac-source-css-property)))
