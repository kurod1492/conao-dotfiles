;;; 01_core-emacs.el ---                             -*- lexical-binding: t; -*-

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

;; Storage allocation and gc for GNU Emacs Lisp interpreter.
(leaf alloc.c
  :require nil
  :config
  ;; pure-bytes-used => using memory
  (setq gc-cons-threshold (* 128 1024 1024) ; memmory allocate with 128MB
        garbage-collection-messages t       ; show message when GC
        ))

;; Asynchronous timers.
(leaf atimer.c
  :require nil)

(leaf indent.c
  :require nil
  :config
  (setq-default indent-tabs-mode nil))

(provide '01_core-emacs)
;;; init.el ends here
