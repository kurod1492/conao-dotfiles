;;; 00_leaf.el ---                                   -*- lexical-binding: t; -*-

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

(mapc (lambda (x)
        (eval
         `(add-to-list
           'load-path
           ,(locate-user-emacs-file (concat "site-lisp/" x)))))
      '("cort.el" "f.el" "s.el" "dash.el" "use-package"
        "feather.el" "leaf.el" "org-mode" "orglyth.el" "emacs-htmlize"))

(require 'leaf)
(require 'use-package)

(load-theme 'wombat)

(provide '00_leaf)
;;; 00_leaf.el ends here
