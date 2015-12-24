;;; 01_core-emacs.el

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:11:08>
;; Last-Updated: <2015/12/24 15:26:17>
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

(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))

(defun add-to-load-path (&rest paths)
  (let (path)
	(dolist (path paths paths)
	  (let ((default-directory
			  (expand-file-name (concat user-emacs-directory path))))
		(add-to-list 'load-path default-directory)
		(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
			(normal-top-level-add-subdirs-to-load-path))))))
(add-to-load-path "lisp" "site-lisp" "conf")

;; coding system
(set-language-environment "Japanese")
(set-default-coding-systems   'utf-8-unix)
(prefer-coding-system         'utf-8-unix)
(set-file-name-coding-system  'utf-8-unix)
(set-keyboard-coding-system   'utf-8-unix)
(setq locale-coding-system    'utf-8-unix)
(setq default-process-coding-system '(undecided-dos . utf-8-unix))

;; set current directory
(cd "~/.emacs.d/conf")

;; don't create file like *.~
(setq make-backup-files nil)
;; don't create file like .#*
(setq auto-save-default nil)
;; don't create backup file
(setq vc-make-backup-files nil)

;;; don't show splash screen
(setq inhibit-splash-screen t)

;; question from emacs
(fset 'yes-or-no-p 'y-or-n-p)

;; reload file after renew
(global-auto-revert-mode 1)

;;; complication ignore Upper or Lower
;; version <= 23.1
(setq completion-ignore-case t)
;; newer emacs
(setq read-buffer-completion-ignore-case t)
(setq read-file-name-completion-ignore-case t)

;; isearch
;; ignore case
(setq-default case-fold-search nil)
(defadvice isearch-mode (around isearch-mode-default-string (forward &optional regexp op-fun recursive-edit word-p) activate)
  (if (and transient-mark-mode mark-active (not (eq (mark) (point))))
      (progn
        (isearch-update-ring (buffer-substring-no-properties (mark) (point)))
        (deactivate-mark)
        ad-do-it
        (if (not forward)
            (isearch-repeat-backward)
          (goto-char (mark))
          (isearch-repeat-forward)))
    ad-do-it))

;; kill whole line
(setq kill-whole-line t)

;; scroll-step
(setq scroll-step 1)

;; stop beep and flash display
(setq ring-bell-function 'ignore)

;; when occurs garbage collection
(setq gc-cons-threshold (* 128 1024 1024))

;;; 右から左に読む言語に対応させないことで描画高速化
(setq-default bidi-display-reordering nil)

;; us mode when emacs start
;(add-hook 'after-init-hook 'mac-change-language-to-us)
;; us mode when mini-buffer
;(add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)
;; us mode when iserach
;(add-hook 'isearch-mode-hook 'mac-change-language-to-us)

;; Emacs serverを起動
(if window-system (server-start))

;; C-i to normal bind
(setq local-function-key-map (delq '(kp-tab . [9]) local-function-key-map))
