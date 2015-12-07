;;; 99_shortcut.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:25:01>
;; Last-Updated: <2015/12/03 03:55:22>
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
(global-set-key (kbd "C-c b") 'battery)
;(global-set-key (kbd "M-g") 'goto-line)
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


;; 11_flymake.el
;; jamp to err, worn line
(global-set-key (kbd "M-p") 'flymake-goto-prev-error)
(global-set-key (kbd "M-n") 'flymake-goto-next-error)
;; show error line
(global-set-key (kbd "C-c u") 'flymake-show-and-sit)

;;; 13_yasnippet.el
;; 既存スニペットを挿入する
(define-key yas-minor-mode-map (kbd "C-c i") 'yas-insert-snippet)
;; 新規スニペットを作成するバッファを用意する
(define-key yas-minor-mode-map (kbd "C-c n") 'yas-new-snippet)
;; 既存スニペットを閲覧・編集する
(define-key yas-minor-mode-map (kbd "C-c e") 'yas-visit-snippet-file)
;; helm連携
(define-key yas-minor-mode-map (kbd "C-c h") 'my-yas/prompt)

;;; 18_elscreen.el
;; C-z c	新規スクリーンを作成して移動する
;; C-z k	現在のスクリーンを閉じる
;; c-z p	前のスクリーンへ
;; c-z n	次のスクリーンへ
;; c-z a	前と次のスクリーンをトグル
;; c-z [0-9]	番号のスクリーンへ
;; c-z ?	ヘルプを表示する

(define-key elscreen-map "\C-k" 'elscreen-kill-screen-and-buffers)
(define-key elscreen-map "k" 'elscreen-kill-screen-and-buffers)

;;; 21_auto-complete.el
(global-set-key (kbd "C-TAB") 'auto-complete)

;;; 30_develop.el
;; open junc file
(global-set-key (kbd "C-x C-z") 'open-junk-file)
;; show discription eval code
(define-key emacs-lisp-mode-map (kbd "C-c C-d") 'lispxmp)

;;; 42_swap.el
;; (global-set-key (kbd "") 'swap-rectangle)
;; (global-set-key (kbd "") 'swap-region)

;;; 50_flycheck.el
;; flymake
(smartrep-define-key
    global-map "M-g" '(("M-n" . 'flymake-goto-next-error)
                       ("M-p" . 'flymake-goto-prev-error)))

;; 80_helm.el
(define-key global-map (kbd "M-x")     'helm-M-x)
(define-key global-map (kbd "C-x C-f") 'helm-find-files)
(define-key global-map (kbd "C-x C-r") 'helm-recentf)
(define-key global-map (kbd "M-y")     'helm-show-kill-ring)
(define-key global-map (kbd "C-c i")   'helm-imenu)
(define-key global-map (kbd "C-x b")   'helm-buffers-list)

(define-key helm-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
(define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
(define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)

;; 30_elscreen.el
(global-set-key (kbd "C-M-<right>") 'elscreen-swap-next)           ; screenの配置位置ずらし(右)
(global-set-key (kbd "C-M-<left>") 'elscreen-swap-previous)        ; screenの配置位置ずらし(左)
(global-set-key [(C-tab)] 'elscreen-next) ; ブラウザみたいに
(global-set-key [(C-S-tab)] 'elscreen-previous) ; ブラウザみたいに　その2
;; C-tab/C-S-tab でスクリーンの切り替え、C-F4 でスクリーンの削除
(global-set-key [C-f4] 'elscreen-kill)
;; M-0 ～ M-9 で指定番号のスクリーンに切り替え
(let ((i 0))
  (while (<= i 9)
    (define-key esc-map (number-to-string i)
                        `(lambda () (interactive) (elscreen-goto ,i)))
    (setq i (1+ i))))

;;; 99_def-function.el
(global-set-key (kbd "C-t") 'other-window-or-split)
(global-set-key (kbd "C-S-t") 'split-window-suitably)
(global-set-key [?\C-,] (lambda () (interactive) (my-operate-buffer nil)))
(global-set-key [?\C-.] (lambda () (interactive) (my-operate-buffer t)))
