;;; 70_conao-lisp.el --- 	

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:52>
;; Last-Updated: <2015/12/12 08:30:10>
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

(use-package split-window
  :bind* (("C-t" . other-window-or-split)
          ("C-S-t" . split-window-suitably)))

(use-package elscreen-swap
  :bind* (("C-M-S-<right>" . elscreen-swap-next)
          ("C-M-S-<left>"  . elscreen-swap-previous)))

(use-package auto-complete-setup
  :commands (conao-ac-common-setup
             conao-ac-emacs-lisp-setup
             conao-ac-cc-mode-setup
             conao-ac-css-mode-setup))

(use-package view-next-file
  :config (progn
            (bind-keys :map dired-mode-map
                       ("C-v" . conao-dired-view-mode))
            (bind-keys :map view-mode-map
                       ("N" . conao-view-next-file-in-dired)
                       ("P" . conao-view-prev-file-in-dired))))

(use-package modeline-replace
  :config (add-hook after-change-major-mode-hook 'clean-mode-line))

(use-package elscreen-start-with-1
  :config (add-hook 'after-init-hook 'my-elscreen-kill-0))

(use-package other-function)
