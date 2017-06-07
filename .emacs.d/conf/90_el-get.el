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

;; (use-package el-get :ensure t
;;   :config
;;   (el-get-bundle el-get)
;;   (el-get-bundle! linkd)
;;   (el-get-bundle conao/auto-save-buffers)
;;   (el-get-bundle conao/other-window-or-split))
;; (use-package el-get
;;   :init
;;   (setq el-get-dir "~/.emacs.d/el-get/")
;;   (unless (require 'el-get nil 'noerror)
;;     (with-current-buffer
;;       (url-retrieve-synchronously
;;        "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
;;       (goto-char (point-max))
;;       (eval-print-last-sexp)))
;;   :config
  
;;   (el-get-bundle el-get)
;;   (el-get-bundle! linkd)
;;   (el-get-bundle conao/auto-save-buffers)
;;   (el-get-bundle conao/other-window-or-split)
  
;;   (use-package auto-save-buffers
;;     :bind (("C-x a s" . auto-save-buffers-toggle))
;;     :config
;;     ;; save buffer 0.5s each
;;     (run-with-idle-timer 0.5 t 'auto-save-buffers))
;;   (use-package other-window-or-split
;;     :bind* ("C-t" . other-window-or-split))

;;   (el-get 'sync)

;;   )

(provide '90_el-get)
;;; 90_el-get.el ends here












