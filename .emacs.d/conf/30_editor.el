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
  :custom ((shackle-rules . '(("\*helm" :regexp t :align below :popup t :size 0.4))))
  :config  
  (shackle-mode 1))

(leaf helm
  :ensure t
  :require t
  :bind (("M-x"     . helm-M-x)
         ("M-X"     . execute-extended-command)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b"   . helm-buffers-list)
         ("C-x C-b" . helm-buffers-list)
         ("C-s"     . helm-occur)
         ("C-x g"   . helm-google-suggest)
         ("C-R"     . helm-regexp)
         ("M-y"     . helm-show-kill-ring)
         ("C-c h"   . helm-command-prefix)
         ;; :map helm-command-map
         ;; ("o"       . helm-occur)
         :map helm-map
         ("<tab>"   . helm-execute-persistent-action)
         ("C-i"     . helm-execute-persistent-action)
         ("C-z"     . helm-select-action))
  :custom (;; open helm buffer inside current window, not occupy whole other window
           (helm-split-window-inside-p            . t)

           ;; move to end or beginning of source when reaching top or bottom of source.
           (helm-move-to-line-cycle-in-source     . t)

           ;; search for library in `require' and `declare-function' sexp.
           (helm-ff-search-library-in-sexp        . t)

           ;; scroll 8 lines other window using M-<next>/M-<prior>
           (helm-scroll-amount                    . 8)
           (helm-ff-file-name-history-use-recentf . t)
           (helm-echo-input-in-header-line        . nil)
           
           (helm-autoresize-max-height . 0)
           (helm-autoresize-min-height . 40)

           ;; 文字列を入力してから検索するまでのタイムラグ。デフォルトで 0
           (helm-input-idle-delay       . 0.0)
           
           ;; 表示する最大候補数。デフォルトで 100
           (helm-candidate-number-limit . 100))
  :config
  (leaf helm-config
    :require t
    :custom ((helm-command-prefix-key "C-c C-h")))
  
  (helm-autoresize-mode t)
  (helm-mode 1)

  ;; Change helm-command-prefix "C-x c" to "c-c h"
  ;; default "C-x c" is quite close to "C-x C-c" which quits Emacs
  ;;  (global-set-key (kbd "C-c h") helm-command-map)
  (global-unset-key (kbd "C-x c")))

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
  
  :custom ((ac-auto-start . 1)                 ; min char to start
           (ac-auto-show-menu . t)             ; show menu immidiately
           (ac-use-fuzzy t)                  ; use fuzzy
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
