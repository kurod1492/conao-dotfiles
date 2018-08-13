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

(defmacro defmacro_nil (name arglist &optional docstring &rest body)
  "Defmacro, return nil.
NAME, ARGLIST, DOCSTRING, BODY"
  `(progn
    (defmacro ,name ,arglist
      ,body)
    nil))

(straight-use-package 'use-package)
(use-package straight
  :config
  (setq straight-use-package-by-default t))

(use-package use-package
  :config
  (setq use-package-expand-minimally   t
        use-package-compute-statistics nil)
  (prog1 "add :doc keyword"
    (add-to-list 'use-package-keywords :doc)
    (defun use-package-normalize/:doc (name keyword args)
      "Do nothing"
      nil)
    (defun use-package-handler/:doc (name keyword arg rest state)
      "Do nothing"
      (let ((body (use-package-process-keywords name rest state)))
        body)))
  
  (prog1 "add :tag keyword"
    (add-to-list 'use-package-keywords :tag)
    (defun use-package-normalize/:tag (name keyword args)
      "Do nothing"
      nil)
    (defun use-package-handler/:tag (name keyword arg rest state)
      "Do nothing"
      (let ((body (use-package-process-keywords name rest state)))
        body))))

(use-package el-get
  :if (or (executable-find "git")
          (message "'git' couldn't found. el-get can't download any packages")
          (defmacro el-get (&rest arg) nil))
  :config
  (setq el-get-git-shallow-clone  t
        el-get-emacswiki-base-url "http://www.emacswiki.org/emacs/download/"))

(use-package use-package-chords
  :init
  (use-package key-chord
    :init ;; (el-get-bundle zk-phi/key-chord)
    :config
    (setq key-chord-two-keys-delay 0.15
          key-chord-safety-interval-backward 0.1
          key-chord-safety-interval-forward  0.25)
    (key-chord-mode 1)))

(use-package smartrep)

(provide '00_use-package)
;;; 00_use-package.el ends here
