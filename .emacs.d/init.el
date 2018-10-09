;;; init.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
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
(setq debug-on-error t)

;; if you run like 'emacs -q -l ~/hoge/init.el'
(progn
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name))))
  (setq user-emacs-directory
	(format "%slocal/%s/" user-emacs-directory emacs-version)))

(defvar init-root-emacs-directory
  (file-name-directory (directory-file-name user-emacs-directory))
  "Example: \"Users/conao/.emacs.d/\"")

(ignore-errors
  (load (format "%sinit/%s.el" init-root-emacs-directory emacs-version)))

(provide 'init)
;;; init.el ends here
