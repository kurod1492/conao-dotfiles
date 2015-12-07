;;; 12_flyspell.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/22 21:22:18>
;; Last-Updated: <2015/11/30 17:14:14>
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
(setq-default ispell-program-name "aspell")
(eval-after-load "ispell"
  '(add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))


(global-set-key (kbd "<f12>") 'flyspell-mode)
(global-set-key (kbd "<f10>") 'flyspell-buffer)
(global-set-key (kbd "<f9>") 'ispell-word)

(mapc                                   ;; flyspell-mode
 (lambda (hook)
   (add-hook hook 'flyspell-prog-mode))
 '(
   c++-mode-hook
   emacs-lisp-mode-hook
   ruby-mode-hook
   python-mode-hook
   ))
(mapc
 (lambda (hook)
   (add-hook hook
             '(lambda () (flyspell-mode 1))))
 '(
   yatex-mode-hook
   ))
;; M-x ispell-complete-word
