;;; 20_editor.El ---

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:37:47>
;; Last-Updated: <2016/02/01 15:35:19>
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

;; NOT use bind, bind* keyword in this file!!
;; those file must be load at emacs startup.

;;; Code:
(use-package auto-complete-config
  ;; :ensure t ;contain auto-complete
  :diminish (auto-complete-mode . "AC")
  :init   (progn
            (use-package pos-tip       :ensure t)
            (use-package fuzzy         :ensure t)
            (use-package auto-complete :ensure t))
  :config (progn
            (ac-config-default)
            (global-auto-complete-mode t)
            (ac-flyspell-workaround)
            (setq ac-auto-start 1
                  ac-delay 0.0
                  ;; ac-use-menu-map t
                  ac-use-fuzzy t
                  ac-ignore-case 't
                  ac-dwim t)))

(use-package undohist
  :ensure t
  :config (progn
            (setq undo-limit 500000
                  undo-strong-limit 500000
                  history-length 4000
                  message-log-max 10000)
            (savehist-mode 1)
            (undohist-initialize)))

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "UT")
  :config   (progn
              (global-undo-tree-mode t)
              (bind-keys
               ("C-x u" . undo-tree-visualize))))

(use-package redo+
  ;; :ensure t ; fetch error at 2015/12/16
  :bind   (("C-M-/" . redo)))

(use-package flycheck
  :ensure t
  :config (progn
            (use-package flycheck-pos-tip :ensure t)
            (custom-set-variables
             '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
            (hook-into-modes #'flycheck-mode
                             'python-mode-hook
                             'perl-mode-hook
                             'emacs-lisp-mode-hook
                             'c-mode-common-hook
                             'ruby-mode-hook)))
