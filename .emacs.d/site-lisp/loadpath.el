;;; loadpath.el ---                                  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@245-073.vpn.hiroshima-u.ac.jp>
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

(provide 'loadpath)
;;; loadpath.el ends here
