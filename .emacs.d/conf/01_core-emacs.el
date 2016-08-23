;;; 01_core-emacs.el

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
(add-to-load-path "site-lisp" "conf")

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
