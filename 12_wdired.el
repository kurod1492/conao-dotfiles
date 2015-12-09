;;; 14_wdired.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/24 13:30:07>
;; Last-Updated: <2015/12/08 09:45:53>
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
(require 'wdired)
(define-key dired-mode-map "r" 'wdired-change-to-wdired-mode)
;; ファイルを削除したらゴミ箱へ
(setq delete-by-moving-to-trash t)

;; (defun my-dired-do-quicklook ()
;;   "In dired, preview with Quick Look."
;;   (interactive)
;;   (let ((file (dired-get-filename))
;;         (process (get-process "qlmanage_ps")))
;;     (if process
;;         (kill-process process)
;;       (start-process "qlmanage_ps" nil "qlmanage" "-p" file))))

;; (add-hook 'dired-mode-hook
;;           '(lambda ()
;;              (define-key dired-mode-map (kbd "SPC") 'my-dired-do-quicklook)
;;              ))