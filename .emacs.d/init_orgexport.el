;;; 99_orgexport.el ---                              -*- lexical-binding: t; -*-

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

(progn
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name))))
  (setq user-emacs-directory
        (concat user-emacs-directory "v" (int-to-string emacs-major-version) "/")))

(load-file (concat user-emacs-directory "site-lisp/loadpath.el"))
(load-file (concat user-emacs-directory "site-lisp/version.el"))

(eval-when-compile (require 'cl-lib))

(require 'package)
(setq package-user-dir (user-setting-directory "elpa"))
(package-initialize)

(require 'org)
(require 'ox)
(require 'cl)  
(setq org-export-async-debug nil)

(provide '99_orgexport)
;;; 99_orgexport.el ends here

