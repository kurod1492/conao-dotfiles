;;; 10_standard-elisp.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 03:11:42>
;; Last-Updated: <2016/01/27 06:05:49>
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

(use-package generic-x
  :defer t
  :config (global-font-lock-mode))

(use-package time-stamp
  ;; %:a -- Monday 曜日
  ;; %#A -- MONDAY 全部大文字で曜日
  ;; %:b -- January 月

  ;; 桁数を指定すると指定した文字だけが表示される.
  ;; "%2#A"なら MO など．
  
  ;; %02H -- 15  時刻 (24 時間)
  ;; %02I -- 03  時刻 (12 時間)
  ;; %#p  -- pm  PM と AM の別
  ;; %P   -- PM  PM と AM の別
  ;; %w   -- 土曜なら 6. 日曜を 0 とし，何番目の曜日なのか
  ;; %02y -- 03  西暦の下 2 桁．
  ;; %z   -- jst  タイムゾーン
  ;; %Z   -- JST  タイムゾーン
  ;; %%   -- %自体を入力
  ;; %f   -- ファイル名
  ;; %F   -- ファイル名のフルパス
  ;; %s   -- マシン名
  ;; %u   -- ログインしたユーザ名
  ;; %U   -- ログインしたユーザのフルネーム 
  ;; :defer t
  :config (progn
            (add-hook 'before-save-hook 'time-stamp)
            (setq time-stamp-active t
                  time-stamp-start "[lL]ast[ -][uU]pdated[ \t]*:[ \t]*<"
                  time-stamp-format "%:y/%02m/%02d %02H:%02M:%02S"
                  time-stamp-end ">"
                  time-stamp-line-limit 20)))

(use-package uniquify
  :defer t
  :config (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package linum
  ;; :defer t
  :config (progn
            (global-linum-mode t)
            (setq linum-delay nil
                  linum-format "%5d")
            (set-face-attribute 'linum nil :height 130)))

(use-package newcomment
  :defer t
  :config (progn
            (setq-default transient-mark-mode t)
            (setq comment-style 'multiline)))

(use-package Flyspell
  :defer t
  :bind* (("<f12>" . flyspell-mode)
          ("<f10>" . flyspell-buffer)
          ("<f9>"  . ispell-word))
  :config (progn 
            (setq-default ispell-program-name "aspell")

            ;; fly-spell
            (mapc
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
               fundamental-mode
               text-mode-hook
               org-mode-hook
               yatex-mode-hook
               ))
            (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+"))))

(use-package wdired
  ;; :defer t
  :config (progn (setq delete-by-moving-to-trash t)
                 (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)))

(use-package view
  :defer t
  :config (progn
            (setcar (cdr (assq 'view-mode minor-mode-alist))
                    (if (fboundp 'propertize)
                        (list (propertize " View"
                                          'face '(:foreground "white"
                                                              :background "DeepPink1")))
                      " View"))

            ;; viewmodeのキーバインドを優先
            (add-hook 'view-mode-hook
                      '(lambda ()
                         (setq minor-mode-Map-Alist
                               (delete (assq 'view-mode minor-mode-map-alist)
                                       minor-mode-map-alist)
                               minor-mode-map-alist
                               (cons (cons 'view-mode view-mode-map) minor-mode-map-alist))
                         ))))

(use-package autoinsert
  ;; :defer t
  :config (progn (setq ;; auto-insert-query nil
                       ;; auto-insert-alist nil
                       auto-insert-directory "~/.emacs.d/template/")
                 (auto-insert-mode 1)))

