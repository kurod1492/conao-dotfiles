;;; 30_utility.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:03>
;; Last-Updated: <2015/12/22 12:48:56>
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
  :bind   (("<f8>" . shell-pop)))

(use-package neotree
  :ensure t
  :defer  t)

(use-package magit
  :ensure t
  :defer  t)

(use-package auto-install
  :ensure t
  :defer  t
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
  :bind (("C-x C-z" . open-junk-file)))

(use-package lispxmp
  :ensure t
  :defer  t
  :bind (("C-c C-d" . lispxmp)))

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

