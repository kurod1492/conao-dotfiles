;;; 01_core-emacs.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@245-073.vpn.hiroshima-u.ac.jp>
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

;; locale settings
(set-language-environment "Japanese")
(prefer-coding-system         'utf-8-unix)
(set-default-coding-systems   'utf-8-unix)
(set-file-name-coding-system  'utf-8-unix)
(set-keyboard-coding-system   'utf-8-unix)
(setq locale-coding-system    'utf-8-unix)
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; file-name coding
(when darwin-p
  (use-package ucs-normalize)
  (setq file-name-coding-system 'utf-8-hfs
        locale-coding-system    'utf-8-hfs))
(when windows-p
  (setq file-name-coding-system 'cp932
        locale-coding-system    'cp932))

(setq make-backup-files          t    ;; *.~
      auto-save-default          t    ;; .#*
      vc-make-backup-files       nil  ;; backup file
      auto-save-list-file-prefix nil  ;; .saves-Macbook-Air.local~
      create-lockfiles           nil) ;; .#aaa.txt@ -> conao@Macbook-Air.local
(setq backup-directory-alist         `((".*" . ,(user-setting-directory "backup/")))
      auto-save-file-name-transforms `((".*" ,(user-setting-directory "backup/") t)))
(setq auto-save-timeout 15
      auto-save-interval 30)

;; don't show splash screen
(setq inhibit-splash-screen t)

;; question from emacs
(fset 'yes-or-no-p 'y-or-n-p)

;; reload file after renew
(global-auto-revert-mode 1)

;; kill whole line
(setq kill-whole-line t)

;; scroll-step
(setq scroll-step 1)

;; stop beep and flash display
(setq ring-bell-function 'ignore)

;; when occurs garbage collection
(setq gc-cons-threshold (* 128 1024 1024))

;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; IME off when focus minibuffer
(when (fboundp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

;; frame title
(setq frame-title-format '("emacs " emacs-version))

;;; window setting
;; hide toolbar, scroll-bar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;; mode-line setting
;; show line number
(line-number-mode t)
;; show column number
(column-number-mode t)
;; show battery force
(display-battery-mode t)

;; truncate
(setq-default truncate-lines t)

;; enlighten editing line
(global-hl-line-mode t)

;;; cursor
;; cursor not blink
(blink-cursor-mode 0)

;; hi-light region
(transient-mark-mode)
(setq highlight-nonselected-windows t)

;; temporary hi-light after yank region
(when window-system
  (defadvice yank (after ys:yank-highlight activate)
    (let ((ol (make-overlay (mark t) (point))))
      (overlay-put ol 'face 'highlight)
      (sit-for 1.0)
      (delete-overlay ol)))
  (defadvice yank-pop (after ys:yank-pop-highlight activate)
    (when (eq last-command 'yank)
      (let ((ol (make-overlay (mark t) (point))))
        (overlay-put ol 'face 'highlight)
        (sit-for 1.0)
        (delete-overlay ol)))))

;; open with drag file
(define-key global-map [ns-drag-file] 'ns-find-file)

;; insert "\", instead "¥"
(define-key global-map [?¥] [?\\])

;; tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; delete region, when yank
(delete-selection-mode t)

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-x r f r : save frame configuration
;; C-x r j r : restore frame configuration

;; Cmd-Ctl-d ; open apple's default dicitonaly

(set-face-attribute 'default nil :family "Monaco" :height 120)
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  (font-spec :family "Hiragino Kaku Gothic ProN"))
(add-to-list 'face-font-rescale-alist
             '(".*Hiragino Kaku Gothic ProN.*" . 1.2))

;; not worn these commands
(put 'downcase-region 'disabled nil)
(put 'upcase-region   'disabled nil)
;; use C-h as DEL
(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

(provide '01_core-emacs)
;;; 01_core-emacs.el ends here
