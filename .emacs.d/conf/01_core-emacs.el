;;; 01_core-emacs.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Naoya Yamashita

;; Author: Naoya Yamashita <conao@Naoya-MacBook-Air.local>
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

;;;;; core-emacs-settings
;; coding system
(set-language-environment "Japanese")
(set-default-coding-systems   'utf-8-unix)
(prefer-coding-system         'utf-8-unix)
(set-file-name-coding-system  'utf-8-unix)
(set-keyboard-coding-system   'utf-8-unix)
(setq locale-coding-system    'utf-8-unix)
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; don't create file like *.~
(setq make-backup-files nil)
;; don't create file like .#*
(setq auto-save-default nil)
;; don't create backup file
(setq vc-make-backup-files nil)
;; don't create .saves-Macbook-Air.local~
(setq auto-save-list-file-prefix nil)
;; don't create .#aaa.txt@ -> conao@Macbook-Air.local
(setq create-lockfiles nil)

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
(mac-auto-ascii-mode t)

;;;;; frame-settings
;; frame title
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

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

;; display line and char count
(defun count-lines-and-chars ()
  "Count line and chars, use in mode line."
  (if mark-active
      (format "%d lines, %d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))

;;;;; window-settings
;; show parent
(show-paren-mode t)

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

;; save buffer condition
(desktop-save-mode t)

;;;;; buffer-settings
;; insert "\", instead "¥"
(define-key global-map [?¥] [?\\])

;; tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; delete region, when yank
(delete-selection-mode t)

;; comment style
(setq comment-style 'multi-line)

;;;;; shortcut-settings
(bind-keys ("C-c a"   . align)
           ("C-c S-a" . align-regexp)
           ("C-c d"   . delete-trailing-whitespace)
           ("C-c b"   . battery)
           ("C-x e"   . eval-last-sexp)
           ("M-r"     . query-replace)
           ("M-c"     . c-mode))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-x r f r : save frame configuration
;; C-x r j r : restore frame configuration

;; Cmd-Ctl-d ; open apple's default dicitonaly

(provide '01_core-emacs)
;;; 01_core-emacs.el ends here
