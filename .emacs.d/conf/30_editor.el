;;; 20_editor.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords: .emacs

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

(leaf shackle
  :ensure t
  :config
  (setq shackle-rules '(("*Help*" :align below :popup t :size 0.3 :select t)
                        ("\*helm" :regexp t :align below :popup t :size 0.4)))
  (shackle-mode 1))

(leaf helm
  :ensure t
  :config
  (leaf helm-config
    :init
    (setq helm-command-prefix-key "C-c C-h"))

  (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action))

(leaf auto-complete
  :ensure t
  :init
  (leaf fuzzy :ensure t)
  :config
  (leaf auto-complete-config)
  (global-auto-complete-mode t)
  
  (define-key ac-mode-map (kbd "TAB") 'ac-trigger-key-command)
  (define-key ac-completing-map (kbd "C-n") 'ac-next)
  (define-key ac-completing-map (kbd "C-p") 'ac-previous)
  
  (setq ac-auto-start 1                 ; min char to start
        ac-auto-show-menu t             ; show menu immidiately
        ac-use-fuzzy t                  ; use fuzzy
        ))

(leaf flycheck
  :ensure t
  :config
  (leaf flycheck-package
    :ensure t
    :init
    (leaf package-lint   ; provide (package-lint-current-buffer)
      :ensure t
      :config
      (leaf package-lint-flymake
        :disabled t
        :ensure t
        :config
        (add-hook 'emacs-lisp-mode-hook #'package-lint-setup-flymake)))
    :config
    (flycheck-package-setup)))
(provide '20_editor)
;;; 20_editor.el ends here
