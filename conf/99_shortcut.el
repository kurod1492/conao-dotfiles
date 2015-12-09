;;; 99_shortcut.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:25:01>
;; Last-Updated: <2015/12/08 13:34:32>
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

;; 01_core-emacs.el
(global-set-key (kbd "C-x e") 'eval-last-sexp)
(global-set-key (kbd "C-c b") 'battery)
;; (global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-r") 'query-replace)
(global-set-key (kbd "M-c") 'c-mode)
(global-set-key (kbd "C-c a") 'align)
(global-set-key (kbd "C-c C-a") 'align-regexp)
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-x r c	clear-rectangle	    矩形領域を空白文字に置き換える
;; C-x r d	delete-rectangle    矩形領域を削除する
;; C-x r o	open-rectangle      空白文字で埋まった矩形領域を挿入する
;; C-x r t	string-rectangle    矩形領域を文字列で置き換える
;; C-x r k	kill-rectangle      矩形領域を削除してキルリングに追加
;; C-x r y	yank-rectangle      キルリングの矩形領域を貼り付ける
;; C-x r r	copy-rectangle-to-register  レジスターに矩形領域を登録する
;; C-x r i	insert-register     レジスターに登録された矩形領域を貼り付ける

;;; 99_def-function.el
(global-set-key (kbd "C-t") 'other-window-or-split)
(global-set-key (kbd "C-S-t") 'split-window-suitably)
(global-set-key [?\C-,] (lambda () (interactive) (my-operate-buffer nil)))
(global-set-key [?\C-.] (lambda () (interactive) (my-operate-buffer t)))

