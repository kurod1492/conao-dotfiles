;;; 00_shortcut.el ---

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 04:59:43>
;; Last-Updated: <2016/03/12 19:46:36>
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
(bind-keys ("C-c a"   . align)
           ("C-c S-a" . align-regexp)
           ("C-c s"   . delete-trailing-whitespace)
           ("C-c b"   . battery)
           ("C-x e"   . eval-last-sexp)
           ("M-r"     . query-replace)
           ("M-c"     . c-mode))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-x r f r : save frame configuration
;; C-x r j r : restore frame configuration
