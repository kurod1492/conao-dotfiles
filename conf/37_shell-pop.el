;;; 37_shell-pop.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Keywords:
;; Created:      <2015/12/01 09:28:17>
;; Last-Updated: <2015/12/01 09:39:02>

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
; shell-pop の設定
(require 'shell-pop)
(global-set-key [f8] 'shell-pop)
;;(shell-pop-set-internal-mode "shell")
;;(shell-pop-set-internal-mode-shell "/bin/bash")
;;(shell-pop-set-window-height 60)
