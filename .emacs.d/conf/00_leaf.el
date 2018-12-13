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
  (declare (indent 1))
  `(progn
     (mapc (lambda (x)
             (add-to-list ,dest-lst x ,append))
           (if ,append ,source-lst (reverse ,source-lst)))
     ,dest-lst))

(defmacro p (form)
  "Output expand given FORM."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))

(defmacro po (form)
  "Output expand given FORM."
  `(progn
     (pp ,form)
     nil))

(eval
 `(add-list-to-list 'load-path
    ',(mapcar (lambda (x)
                (locate-user-emacs-file (format "site-lisp/%s" x)))
              '("cort.el" "feather.el" "leaf.el" "orglyth.el"))))

(require 'leaf)

(leaf bind-key :ensure t)
(leaf package
  :config
  (add-list-to-list 'package-archives
    '(("org"       . "https://orgmode.org/elpa/")
      ("melpa"     . "https://melpa.org/packages/")
      ("marmalade" . "https://marmalade-repo.org/packages/"))))

(leaf s    :ensure t)
(leaf f    :ensure t)
(leaf dash :ensure t)

(provide '00_leaf)
;;; 00_leaf.el ends here
