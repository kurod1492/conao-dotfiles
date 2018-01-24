;;; init.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@naoya-imac.local>
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

;; if you run like 'emacs -q -l ~/hoge/init.el'
;; load settings in ~/hoge/
(if load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name))
  (setq user-emacs-directory "~/.emacs.d/"))

(defmacro user-setting-directory (directory)
  "Return user-emacs-directory/DIRECTORY to setting Emacs."
  (concat user-emacs-directory directory))

;; package
(require 'package)
(add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; use-package
(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

;; org-mode
(use-package org :ensure t
  :config
  (setq org-src-preserve-indentation t)
  
  (defvar org-startup-dir (user-setting-directory "conf/"))
  (defun load-org-file (file)
    "load org file"
    (org-babel-load-file (expand-file-name file org-startup-dir)))
  
  (org-babel-load-file "init.org"))

(provide 'init)
;;; init.el ends here
