;;; 30_utility.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:03>
;; Last-Updated: <2015/12/26 13:31:54>
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
(use-package elscreen
  :ensure t
  ;; :defer  t
  :config (progn (setq elscreen-prefix-key "\C-z"
                       elscreen-tab-display-kill-screen nil
                       elscreen-tab-display-control nil
                       elscreen-display-screen-number nil)
                 (bind-key "C-k" 'elscreen-kill-screen-and-buffers elscreen-map)
                 (use-package elscreen-start-with-1
                   :config (add-hook 'after-init-hook 'my-elscreen-kill-0)))
  :bind* (("C-M-<right>" . elscreen-swap-next)
          ("C-M-<left>"  . elscreen-swap-previous)
          ("C-<tab>"     . elscreen-next)
          ("C-S-<tab>"   . elscreen-previous)
          ("C-z d"       . elscreen-dired)
          ("C-z r"       . elscreen-screen-nickname)))

;; use my lisp/elscreen-swap.el
(use-package elscreen-swap
  :bind* (("C-M-S-<right>" . elscreen-swap-next)
          ("C-M-S-<left>"  . elscreen-swap-previous)))

(use-package elscreen-persist
  :ensure t
  ;; :defer  t
  :config (elscreen-persist-mode))

(use-package elscreen-dired  :defer t)
(use-package elscreen-w3m    :defer t)
(use-package elscreen-server :defer t)

(use-package yatemplate
  :ensure t
  ;; :defer  t
  :config (progn (yatemplate-fill-alist)
                 (defun find-file-hook--yatemplate ()
                   "yatemplateのsnippetのテストができるようにするためにsnippet-modeにする"
                   (when (string-match "emacs.*/templates/" buffer-file-name)
                     (let ((mode major-mode))
                       (snippet-mode)
                       (setq-local yas--guessed-modes (list mode)))))
                 (defun after-save-hook--yatemplate ()
                   "yatemplateファイル保存後、auto-insert-alistに反映させる"
                   (when (string-match "emacs.*/templates/" buffer-file-name)
                     (yatemplate-fill-alist)))
                 (add-hook 'find-file-hook 'find-file-hook--yatemplate)
                 (add-hook 'after-save-hook 'after-save-hook--yatemplate)))

(use-package free-keys
  :ensure t
  :defer  t)

(use-package shell-pop
  :ensure t
  :defer  t
  :bind   (("C-o" . shell-pop)))

(use-package neotree
  :disabled t
  :ensure t
  :defer  t)

(use-package magit
  :ensure t
  :defer  t
  :bind  (("C-x v" . magit-status)))

(use-package auto-install
  :ensure t
  :defer  t
  :commands
  auto-install-batch
  auto-install-from-url
  auto-install-from-gist
  auto-install-from-library
  auto-install-from-emacswiki
  :config (setq auto-install-directory "~/.emacs.d/site-lisp"))

(use-package tab-jump-out
  :ensure t
  ;; :defer  t
  :config (setq yas-fallback-behavior '(apply tab-jump-out 1)))

(use-package w3m
  :if (executable-find "w3m")
  :ensure t
  :defer  t)

(use-package open-junk-file
  :ensure t
  :defer  t
  :bind (("C-x C-x" . open-junk-file)))

(use-package lispxmp
  :ensure t
  :defer  t
  :bind (("C-c C-e" . lispxmp)))

(use-package paredit
  :disabled t
  :ensure t
  :defer  t)

(use-package auto-async-byte-compile
  :ensure t
  :defer  t
  :config (progn (setq auto-async-byte-compile-exclude-files-regexp "/junk/"
                       eldoc-idle-delay 0.2
                       eldoc-minor-mode-string "")  ;; dont show ElDoc in mode line
                 (find-function-setup-keys)))

(use-package minibuf-isearch
  :ensure t
  :defer  t)

(use-package sequential-command-config
  ;; :disabled t
  :init   (use-package sequential-command :ensure t)
  :config (progn (sequential-command-setup-keys)))

(use-package smartparens-config
  :init   (use-package smartparens :ensure t)
  ;; :defer t
  :config (progn (smartparens-global-mode)
                 (sp-pair "$" "$")))

(use-package dired-rainbow
  :ensure t
  :defer  t)

(use-package mode-compile
  :ensure t
  :defer  t)

(use-package auto-async-byte-compile
  :ensure t
  :defer  t
  :commands enable-auto-async-byte-compile-mode
  :init   (hook-into-modes #'enable-auto-async-byte-compile-mode
                           'emacs-lisp-mode-hook)
  :config (progn (setq auto-async-byte-compile-exclude-files-regexp "/junk/")))

(use-package session
  :ensure t
  ;; :defer  t
  :config (progn
            (setq session-initialize '(de-saveplace session keys menus places)
                  session-globals-include '((kill-ring 50)
                                            (session-files-alist 500 t)
                                            (file-name-history 10000))
                  session-globals-maxlstring 100000000
                  history-length t
                  session-undo-check -1)
            (add-hook 'after-init-hook 'session-initialize)))

(use-package electric-operator
  :ensure t
  :defer  t
  :commands electric-operator-mode
  :init (progn (hook-into-modes #'electric-operator-mode
                                'c-mode-common-hook)))

(use-package rainbow-mode
  :ensure t
  :defer  t
  :commands rainbow-mode
  :init (progn (hook-into-modes #'rainbow-mode
                                'emacs-lisp-mode-hook
                                'lisp-mode-hook
                                'css-mode-hook
                                'less-mode-hook
                                'web-mode-hook
                                'html-mode-hook)))

(use-package dired-filter
  :ensure t
  :defer  t
  )

(use-package subtree
  :ensure t
  :defer  t
  :init  (use-package dired-details)
  :config (progn
            ;; iを置き換え
            (define-key dired-mode-map (kbd "i") 'dired-subtree-insert)
            ;; org-modeのようにTABで折り畳む
            (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-remove)
            ;; C-x n nでsubtreeにナローイング
            (define-key dired-mode-map (kbd "C-x n n") 'dired-subtree-narrow)

            ;; ファイル名以外の情報を(と)で隠したり表示したり
            (require 'dired-details)
            (dired-details-install)
            (setq dired-details-hidden-string "")
            (setq dired-details-hide-link-targets nil)
            (setq dired-details-initially-hide nil)

            ;; dired-subtreeをdired-detailsに対応させる
            (defun dired-subtree-after-insert-hook--dired-details ()
              (dired-details-delete-overlays)
              (dired-details-activate))
            (add-hook 'dired-subtree-after-insert-hook
                      'dired-subtree-after-insert-hook--dired-details)

            ;; find-dired対応
            (defadvice find-dired-sentinel (after dired-details (proc state) activate)
              (ignore-errors
                (with-current-buffer (process-buffer proc)
                  (dired-details-activate))))
            ;; (progn (ad-disable-advice 'find-dired-sentinel 'after 'dired-details) (ad-update 'find-dired-sentinel))

            ;; [2014-12-30 Tue]^をdired-subtreeに対応させる
            (defun dired-subtree-up-dwim (&optional arg)
              "subtreeの親ディレクトリに移動。そうでなければ親ディレクトリを開く(^の挙動)。"
              (interactive "p")
              (or (dired-subtree-up arg)
                  (dired-up-directory)))
            (define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)))

