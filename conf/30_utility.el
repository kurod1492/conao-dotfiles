;;; 30_utility.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:03>
;; Last-Updated: <2015/12/12 20:53:59>
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
  :defer  t
  :config (progn (setq elscreen-prefix-key "\C-z"
                       elscreen-tab-display-kill-screen nil
                       elscreen-tab-display-control nil
                       elscreen-display-screen-number nil)
                 (bind-key "C-k" 'elscreen-kill-screen-and-buffers elscreen-map))
  :bind (("C-M-<right>" . elscreen-swap-next)
         ("C-M-<left>"  . elscreen-swap-previous)
         ("C-<tab>"     . elscreen-next)
         ("C-S-<tab>"   . elscreen-previous)))

(use-package elscreen-persist
  :ensure t
  :defer  t
  :config (elscreen-persist-mode))

(use-package elscreen-dired  :defer t)
(use-package elscreen-w3m    :defer t)
(use-package elscreen-server :defer t)

(use-package yatemplate
  :ensure t
  :defer  t
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
                 (add-hook 'after-save-hook 'after-save-hook--yatempl)))

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
  ;; not found magit-popup
  ;; :ensure t
  :defer  t)

(use-package auto-install
  :ensure t
  :defer  t
  :config (setq auto-install-directory "~/.emacs.d/site-lisp"))

(use-package tab-jump-out
  :ensure t
  :defer  t
  :config (setq yas-fallback-behavior '(apply tab-jump-out 1)))

(use-package w3m)

