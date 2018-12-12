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

;; (if (assoc 'leaf package-archive-contents)
;;     (package-install package)
;;   (package-refresh-contents)
;;   (package-install package))

;; support funcs
(defmacro add-list-to-list (dest-lst source-lst &optional append)
  "Add SOURCE_LST to DEST-LST in a destructive.
Defaltly, add at the beginning of the list,
but when APPEND is non-nil, SOURCE-LST is added at the end.
This function is minor change from `add-to-list'."
  `(progn
     (mapc (lambda (x)
             (add-to-list ,dest-lst x ,append))
           (if ,append ,source-lst (reverse ,source-lst)))
     ,dest-lst))

;; load-path
(mapc (lambda (x)
        (eval
         `(add-to-list
           'load-path
           ,(locate-user-emacs-file (concat "site-lisp/" x)))))
      '("cort.el" "feather.el" "leaf.el" "orglyth.el"))

(require 'leaf)

(provide '00_leaf)
;;; 00_leaf.el ends here
