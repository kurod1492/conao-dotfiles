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
(progn
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name))))
  (setq user-emacs-directory
        (concat user-emacs-directory "v" (int-to-string emacs-major-version) "/")))

(add-to-list 'load-path (concat user-emacs-directory "site-lisp"))
(require 'loadpath)
(require 'version)

(when emacs22-l-p (error "Unsupport version prior to emacs22"))

;; use cl macros
(when emacs24-g-p (eval-when-compile (require 'cl-lib)))

(defvar load-path-folder-list '("backup")
  "folder-list add to load-path recursive. `user-setting-directory'/`load-path-folder-list'")

(cond (emacs23-p
       (progn
         (add-list-to-list 'load-path-folder-list '("el-get" "site-lisp" "conf"))
         (add-user-setting-directory-to-load-path load-path-folder-list)

         (require 'el-get)
         (setq el-get-git-shallow-clone t)
         (setq el-get-dir (user-setting-directory "el-get"))))
      (emacs24-g-p
       (progn
         (add-list-to-list 'load-path-folder-list '("el-get" "site-lisp" "conf"))
         (add-user-setting-directory-to-load-path load-path-folder-list))))

(cond (emacs23-p
       (progn
         (el-get-bundle emacs-jp/init-loader)))
      (emacs24-g-p
       (progn
         (let ((bootstrap-file
                (user-setting-directory "straight/repos/straight.el/bootstrap.el"))
               (bootstrap-version 5))
           (unless (file-exists-p bootstrap-file)
             (with-current-buffer
                 (url-retrieve-synchronously
                  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
                  'silent 'inhibit-cookies)
               (goto-char (point-max))
               (eval-print-last-sexp)))
           (load bootstrap-file nil 'nomessage))

         (straight-use-package 'init-loader)
         (init-loader-load (user-setting-directory "conf")))))

(provide 'init)
;;; init.el ends here
