;;; init.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@184-187.cup.hiroshima-u.ac.jp>
;; Keywords: .emacs

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
(setq debug-on-error  t
      debug-on-signal nil
      debug-on-quit   nil)

;; if you run like 'emacs -q -l ~/hoge/init.el'
;; load settings in ~/hoge/
(setq user-emacs-directory
      (expand-file-name
       (file-name-directory
        (if load-file-name load-file-name "~/.emacs.d/init.el"))))

(load-file (concat user-emacs-directory "site-lisp/loadpath.el"))
(load-file (concat user-emacs-directory "site-lisp/version.el"))

(when emacs22-l-p (error "unsupport version prior to emacs22"))

(defvar load-path-folder-list '("backup")
  "folder-list add to load-path recursive. `user-setting-directory'/`load-path-folder-list'")

(cond (emacs23-p
       (progn
         (add-list-to-list 'load-path-folder-list '("v23/el-get" "v23/site-lisp" "v23/conf"))
         (add-user-setting-directory-to-load-path load-path-folder-list)

         (require 'el-get)
         (setq el-get-git-shallow-clone t)
         (setq el-get-dir (user-setting-directory "v23/el-get"))))
      (emacs24-p
       (progn
         (add-list-to-list 'load-path-folder-list '("v24/el-get" "v24/elpa" "site-lisp" "conf"))
         (add-user-setting-directory-to-load-path load-path-folder-list)
         
         (require 'package)
         (setq package-user-dir (user-setting-directory "v24/elpa"))))
      (emacs25-p
       (progn
         (add-list-to-list 'load-path-folder-list '("v25/el-get" "v25/elpa" "site-lisp" "conf"))
         (add-user-setting-directory-to-load-path load-path-folder-list)
         
         (require 'package)
         (setq package-user-dir (user-setting-directory "v25/elpa")))))

(cond (emacs23-p
       (progn
         (el-get-bundle emacs-jp/init-loader)))
      (emacs24-g-p
       (progn
         (add-list-to-list 'package-archives '(("melpa"     . "http://melpa.org/packages/")
                                               ("org"       . "http://orgmode.org/elpa/")
                                               ("marmalade" . "http://marmalade-repo.org/packages/")))
         (let ((dir (concat (user-setting-directory "/elpa/") "latex-math-preview-20170522.1455")))
           (if (file-directory-p dir)
               (delete-directory dir t)))

         (package-initialize)


         ;; use-package
         (when (not (package-installed-p 'use-package))
           (package-refresh-contents)
           (package-install 'use-package))

         ;; theme settings
         (use-package solarized-theme :ensure t
           :init
           (load-theme 'solarized-dark t))

         ;; init-loader
         (use-package init-loader :ensure t
           :config
           (init-loader-load (user-setting-directory "conf"))))))

(provide 'init)
;;; init.el ends here
