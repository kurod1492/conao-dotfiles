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

;; enable debug
(setq debug-on-error t
      debug-on-signal nil
      debug-on-quit nil)

;; if you run like 'emacs -q -l ~/hoge/init.el'
;; load settings in ~/hoge/

(setq user-emacs-directory
      (expand-file-name
       (file-name-directory
        (if load-file-name load-file-name "~/.emacs.d/init.el"))))

(defun user-setting-directory (directory)
  "Return user-emacs-directory/DIRECTORY to setting Emacs."
  (concat user-emacs-directory directory))

(defun add-to-load-path (paths)
  "Add load path recursive in PATHS."
  (dolist (path paths)
    (let ((default-directory path))
      (add-to-list 'load-path default-directory)
      (normal-top-level-add-subdirs-to-load-path))))

(defun add-user-setting-directory-to-load-path (dnames)
  "Add load path recursive in $user-setting-directory/FNAMES
DNAMES is directory name list in user-setting-directory"
  (add-to-load-path
   (mapcar #'(lambda (x)
               (let ((path))
                 (setq path (expand-file-name (user-setting-directory x)))
                 (unless (file-directory-p path)
                   (message "mkdir: %s" path)
                   (mkdir path))
                 path))
           dnames)))

(defmacro add-list-to-list (added-lst lst)
  "minor change from add-to-list.
add LST to ADDED-LST in a destructive"
  `(progn
     (mapc (lambda (x)
             (add-to-list ,added-lst x))
           ,lst)
     ,added-lst))

(defvar load-path-folder-list '("conf" "el-get")
  "folder-list add to load-path recursive. `user-setting-directory'/`load-path-folder-list'")

(cond ((= emacs-major-version 23)
       (progn
         (add-list-to-list 'load-path-folder-list '("_elpa-23" "_site-lisp-23"))
         (add-user-setting-directory-to-load-path load-path-folder-list)

         ;; package
         (require 'package-23)
         (add-list-to-list 'package-archives '(("melpa"     . "http://melpa.org/packages/")
                                               ("org"       . "http://orgmode.org/elpa/")
                                               ("marmalade" . "http://marmalade-repo.org/packages/")))
         (setq package-user-dir (user-setting-directory "_elpa-23"))
         (package-initialize)))
      ((>= emacs-major-version 24)
       (progn
         (add-list-to-list 'load-path-folder-list '("elpa" "site-lisp"))
         (add-user-setting-directory-to-load-path load-path-folder-list)
         
         ;; package
         (require 'package)
         (add-list-to-list 'package-archives '(("melpa"     . "http://melpa.org/packages/")
                                               ("org"       . "http://orgmode.org/elpa/")
                                               ("marmalade" . "http://marmalade-repo.org/packages/")))
         (let ((dir (concat (user-setting-directory "elpa/") "latex-math-preview-20170522.1455")))
           (if (file-directory-p dir)
               (delete-directory dir t)))
         (package-initialize)

         ;; use-package
         (when (not (package-installed-p 'use-package))
           (package-refresh-contents)
           (package-install 'use-package))

         ;; el-get
         (use-package el-get :ensure t)

         ;; theme settings
         (use-package solarized-theme :ensure t
           :init
           (load-theme 'solarized-dark t))
         
         ;; babel-loader
         (use-package babel-loader
           :init
           (el-get-bundle takaishi/babel-loader.el)
           (use-package org :ensure t
             :config
             (setq org-src-preserve-indentation t))
           (use-package init-loader :ensure t
             :config
             (setq init-loader-show-log-after-init 'error-only
                   init-loader-byte-compile        nil))
           :config
           (bl:load-dir (user-setting-directory "conf/"))))))
(provide 'init)
;;; init.el ends here
