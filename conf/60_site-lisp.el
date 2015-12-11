;;; 60_site-lisp.Eli --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:37>
;; Last-Updated: <2015/12/12 08:48:30>
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
(use-package auto-save-buffers
  :config (run-with-idle-timer 0.5 t 'auto-save-buffers)
  :bind   (("C-x a s" . auto-save-buffers-toggle)))

(use-package navbarx-elscreen)
(use-package navbarx-version)
(use-package navbarx-time
  :ensure  elscreen
  :config (progn
            (setq navbar-item-list '(navbarx-version
                                     navbarx-time
                                     navbarx-elscreen))
            (navbar-mode)
            (navbar-revive-workaround)
            (display-time-mode)

            ;;: elscreen-start ;;;
            (elscreen-start)))

(use-package open-junk-file
  :bind (("C-x C-z" . open-junk-file)))

(use-package lispxmp
  :bind (("C-c C-d" . lispxmp)))

(use-package paredit
  :disabled t
  :defer t)

(use-package auto-async-byte-compile
  :defer t
  :config (progn (setq auto-async-byte-compile-exclude-files-regexp "/junk/"
                       eldoc-idle-delay 0.2
                       eldoc-minor-mode-string "")  ;; dont show ElDoc in mode line
                 (find-function-setup-keys)))

(use-package minibuf-isearch
  :defer t)

(use-package guide-key-tip
  :defer t)

(use-package sequential-command-config
  :disabled t
  :config (sequential-command-setup-keys))

(use-package smartparens-config
  :defer t
  :config (progn (smartparens-global-mode)
                 (sp-pair "$" "$")))
