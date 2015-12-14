;;; 20_editor.Eli --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:37:47>
;; Last-Updated: <2015/12/14 15:13:59>
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
(use-package pos-tip
  :ensure t
  :defer  t)

(use-package yasnippet
  :ensure t
  ;; :defer  t
  :diminish (yas-minor-mode . "YS")
  :config (progn
            (setq yas-snippet-dirs
                  '("~/.emacs.d/mySnippets"
                    "~/.emacs.d/crottiSnippets"
                    "~/.emacs.d/elpa/yasnippet-20151108.1505/snippets"
                    ))
            (yas-global-mode 1)
            (bind-keys :map yas-minor-mode-map
                       ("C-c i" . yas-insert-snippet)
                       ("C-c n" . yas-new-snippet)
                       ("C-c e" . yas-visit-snippet-file))))

(use-package auto-complete-config
  ;; :ensure fuzzy
  ;; :defer  t
  :diminish (auto-complete-mode . "AC")
  :config (progn
            (ac-config-default)
            (ac-flyspell-workaround)
            (setq ac-auto-start 1
                  ac-delay 0.0
                  ;; ac-use-menu-map t
                  ac-use-fuzzy t
                  ac-ignore-case 't
                  ac-dwim t))
  :bind (("C-M-?" . ac-last-help)))

(use-package undohist
  :ensure t
  ;; :defer  t
  :config (progn
            (setq undo-limit 500000
                  undo-strong-limit 500000
                  history-length 4000
                  message-log-max 10000)
            (savehist-mode 1)
            (undohist-initialize)))

(use-package undo-tree
  :ensure t
  :defer  t
  :diminish (undo-tree-mode . "UT")
  :config (global-undo-tree-mode t)
  :bind   (("C-x u" . undo-tree-visualize)))

(use-package redo+
  :ensure t
  :defer  t
  :bind   (("C-M-/" . redo)))

(use-package flycheck
  :ensure t
  ;; :defer  t)
  )

(use-package electric-operator
  :ensure t
  :defer  t
  :config (progn (add-hook 'c-mode-common-hook 'electric-operator-mode)))
