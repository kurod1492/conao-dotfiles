;;; 23_auto-complete.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:22:31>
;; Last-Updated: <2015/12/08 13:18:19>
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
(require 'auto-complete)
(require 'auto-complete-config)
;; デフォルトの設定ではなく、自前で実装する。
;; (ac-config-default)

(global-auto-complete-mode t)

(add-to-list 'ac-modes 'text-mode)         ;; text-modeでも自動的に有効にする
(add-to-list 'ac-modes 'fundamental-mode)  ;; fundamental-mode
(add-to-list 'ac-modes 'org-mode)
(add-to-list 'ac-modes 'yatex-mode)

;; ;; expand share spelling in completion menu
(setq ac-expand-on-auto-complete t)

;; renew immediately
(setq ac-delay 0.0)
(setq ac-auto-start 1)
(ac-set-trigger-key "TAB")
(setq ac-use-menu-map t)       ;; 補完メニュー表示時にC-n/C-pで補完候補選択
(setq ac-use-fuzzy t)          ;; 曖昧マッチ
(setq ac-ignore-case 't)       ;; 大文字小文字を区別しない

;; auto-completeとflyspellの競合回避
(ac-flyspell-workaround)
;; 表示を崩れにくくする
(setq popup-use-optimized-column-computation nil)

;; ac-completion-face	インライン補完の文字色
;; ac-candidate-face	補完メニューの背景色
;; ac-selection-face	補完メニューの選択色
;; (set-face-background 'ac-candidate-face "lightgray")
;; (set-face-underline 'ac-candidate-face "darkgray")
;; (set-face-background 'ac-selection-face "steelblue")

;; what do I mean
;; 補完選択時にTABがRETの挙動に変化する
(setq ac-dwim t)

;; complement menu
(setq ac-auto-show-menu 0.0)
(setq ac-menu-height 10)

;;( ac-source-features
;;  ac-source-functions
;;  ac-source-yasnippet
;;  ac-source-variables
;;  ac-source-symbols
;;  ac-source-abbrev
;;  ac-source-dictionary
;;  ac-source-words-in-same-mode-buffers)

(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(defun list-system-header-files ()
  (directory-files "/usr/include" nil "^.*\\.h$"))

(defvar system-header-files-cache (list-system-header-files))

(defvar ac-source-system-header-files
  '((candidates . system-header-files-cache)))

(defvar ac-source-to-mailaddr
  '((candidates . (list "foo1@example.com"
                        "foo2@example.com"
                        "foo3@example.com"))
    (prefix . "^To: \\(.*\\)")))
;;(setq ac-sources '(ac-source-to-mailaddr))

;;; shortcut
(global-set-key (kbd "C-TAB") 'auto-complete)
