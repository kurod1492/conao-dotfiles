;;; 32_develop.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:37:29>
;; Last-Updated: <2015/12/01 16:26:04>
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

;; open junc file
(require 'open-junk-file)

;; show discription eval code
(require 'lispxmp)

;; edit contain parent pair
;;(require 'paredit)

;; disable auto bite compile
(require 'auto-async-byte-compile)
(setq auto-async-byte-compile-exclude-files-regexp "/junk/")
(setq eldoc-idle-delay 0.2)
(setq eldoc-minor-mode-string "") ;;dont show ElDoc in mode line

(find-function-setup-keys)

