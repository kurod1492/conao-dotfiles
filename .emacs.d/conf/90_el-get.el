;;; 90_el-get.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Naoya Yamashita

;; Author: Naoya Yamashita <conao@Naoya-MacBook-Air.local>
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

;(setq el-get-dir "~/.emacs.d/el-get/")

(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (goto-char (point-max))
    (eval-print-last-sexp)))
;; (el-get 'sync)

;; (el-get-bundle! linkd)

(use-package auto-save-buffers
  :init
  (unless (require 'auto-save-buffers nil 'noerror)
    (el-get-bundle conao/auto-save-buffers))
  :bind (("C-x a s" . auto-save-buffers-toggle))
  :config
  (run-with-idle-timer 0.5 t 'auto-save-buffers)) ; アイドル0.5秒で保存

(use-package other-window-or-split
  :init
  (unless (require 'other-window-or-split nil 'noerror)
    (el-get-bundle conao/other-window-or-split))
  :bind* ("C-t" . other-window-or-split))

(provide '90_el-get)
;;; 90_el-get.el ends here












