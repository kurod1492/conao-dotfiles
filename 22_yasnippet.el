;;; 21_yasnippet.el --- yasnippet

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 14:27:00>
;; Last-Updated: <2015/12/08 13:16:39>
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

;; 自分用・追加用テンプレート -> mysnippetに作成したテンプレートが格納される
(require 'yasnippet)
(setq yas-snippet-dirs
      '("~/.emacs.d/mySnippets"
        "~/.emacs.d/crottiSnippets"
        "~/.emacs.d/elpa/yasnippet-20151108.1505/snippets"
        ))

(yas-global-mode 1)

;;; shortcut
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-c i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-c n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-c e") 'yas-visit-snippet-file)
;; helm連携
(define-key yas-minor-mode-map (kbd "C-c h") 'my-yas/prompt)

