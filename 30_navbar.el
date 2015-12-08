;;; 46_navbar.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Keywords: 
;; Created:      <2015/12/04 00:47:05>
;; Last-Updated: <2015/12/08 13:22:06>

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
;; https://github.com/papaeye/emacs-navbar
(require 'navbarx-elscreen)
(require 'navbarx-version)
(require 'navbarx-time)

(setq navbar-item-list '(navbarx-version navbarx-time navbarx-elscreen))
(navbar-mode)
(navbar-revive-workaround)

(display-time-mode)

;; find color like color theme
(require 'elscreen-color-theme)

;; persist elscreen
;; (toggle-frame-fullscreen)
(require 'elscreen-persist)
(elscreen-persist-mode 1)

;; open new tab to press 'o'
(require 'elscreen-dired)

;; control w3m with elscreen
(require 'elscreen-w3m)

;; create screen when open emacs client
(require 'elscreen-server)

(setq elscreen-prefix-key "\C-z")
;; タブの先頭に[X]を表示しない
(setq elscreen-tab-display-kill-screen nil)
;; header-lineの先頭に[<->]を表示しない
(setq elscreen-tab-display-control nil)
;; バッファ名・モード名からタブに表示させる内容を決定する(デフォルト設定)
(setq elscreen-buffer-to-nickname-alist
      '(("^dired-mode$" .
         (lambda ()
           (format "Dired(%s)" dired-directory)))
        ("^Info-mode$" .
         (lambda ()
           (format "Info(%s)" (file-name-nondirectory Info-current-file))))
        ("^mew-draft-mode$" .
         (lambda ()
           (format "Mew(%s)" (buffer-name (current-buffer)))))
        ("^mew-" . "Mew")
        ("^irchat-" . "IRChat")
        ("^liece-" . "Liece")
        ("^lookup-" . "Lookup")))
(setq elscreen-mode-to-nickname-alist
      '(("[Ss]hell" . "shell")
        ("compilation" . "compile")
        ("-telnet" . "telnet")
        ("dict" . "OnlineDict")
        ("*WL:Message*" . "Wanderlust")))

;; Don't show tab number in mode-line
(setq elscreen-display-screen-number nil)

;;;
;;; elscreen のタブの並びと数字キーの並びを合わせる
;;;

;; 既存スクリーンのリストを要求された際、0 番が存在しているかのように偽装する
(defadvice elscreen-get-screen-list (after my-ad-elscreen-get-screenlist disable)
  (add-to-list 'ad-return-value 0))

;; スクリーン生成時に 0 番が作られないようにする
(defadvice elscreen-create (around my-ad-elscreen-create activate)
  (interactive)
  ;; 0 番が存在しているかのように偽装
  (ad-enable-advice 'elscreen-get-screen-list 'after 'my-ad-elscreen-get-screenlist)
  (ad-activate 'elscreen-get-screen-list)
  ;; 新規スクリーン生成
  ad-do-it
  ;; 偽装解除
  (ad-disable-advice 'elscreen-get-screen-list 'after 'my-ad-elscreen-get-screenlist)
  (ad-activate 'elscreen-get-screen-list))

;; スクリーン 1 番を作成し 0 番を削除 (起動時、フレーム生成時用)
(defun my-elscreen-kill-0 ()
  (when (and (elscreen-one-screen-p)
             (elscreen-screen-live-p 0))
    (elscreen-create)
    (elscreen-kill 0)))

;; フレーム生成時のスクリーン番号が 1 番になるように
(defadvice elscreen-make-frame-confs (after my-ad-elscreen-make-frame-confs activate)
  (let ((selected-frame (selected-frame)))
    (select-frame frame)
    (my-elscreen-kill-0)
    (select-frame selected-frame)))

;; 起動直後のスクリーン番号が 1 番になるように
(add-hook 'after-init-hook 'my-elscreen-kill-0)

;; start elscreen
(elscreen-start)

;;; shortcut
;; C-z c	新規スクリーンを作成して移動する
;; C-z k	現在のスクリーンを閉じる
;; c-z p	前のスクリーンへ
;; c-z n	次のスクリーンへ
;; c-z a	前と次のスクリーンをトグル
;; c-z [0-9]	番号のスクリーンへ
;; c-z ?	ヘルプを表示する

(define-key elscreen-map "\C-k" 'elscreen-kill-screen-and-buffers)
(global-set-key (kbd "C-M-<right>") 'elscreen-swap-next)           ; screenの配置位置ずらし(右)
(global-set-key (kbd "C-M-<left>") 'elscreen-swap-previous)        ; screenの配置位置ずらし(左)
(global-set-key [(C-tab)] 'elscreen-next) ; ブラウザみたいに
(global-set-key [(C-S-tab)] 'elscreen-previous) ; ブラウザみたいに　その2
