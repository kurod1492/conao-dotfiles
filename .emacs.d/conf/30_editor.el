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

(leaf simple-httpd :ensure t)

(leaf real-auto-save
  :ensure t
  :custom ((real-auto-save-interval . 0.3))
  :commands real-auto-save-mode
  :hook (find-file-hook . real-auto-save-mode))

(leaf hungry-delete :ensure t
      :config (global-hungry-delete-mode))

(leaf shackle
  :ensure t
  :custom ((shackle-rules . '(("\*helm" :regexp t :align below :popup t :size 0.4))))
  :config  
  (shackle-mode 1))

(leaf yasnippet
  :ensure t
  :bind (:map yas-minor-mode-map
              ("C-c y i" . yas-insert-snippet)
              ("C-c y n" . yas-new-snippet)
              ("C-c y v" . yas-visit-snippet-file)
              ("C-c y l" . yas-describe-tables)
              ("C-c y g" . yas-reload-all))
  :config (yas-global-mode 1))

(leaf treemacs :ensure t)
(leaf company :ensure t)
(leaf lsp-mode
  :ensure t
  :custom ((lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (create-lockfiles nil))
  :hook (prog-major-mode . lsp-prog-major-mode-enable)
  :config
  (leaf lsp-ui
    :ensure t
    :hook (lsp-mode-hook . lsp-ui-mode))
  (leaf company-lsp
    :ensure t
    :config
    (add-to-list 'company-backends 'company-lsp)
    ;; :after (lsp-mode company yasnippet)
    ;; :custom
    ;; ((company-lsp-cache-candidates t) ;; auto, t(always using a cache), or nil
    ;;  (company-lsp-async t)
    ;;  (company-lsp-enable-snippet t)
    ;;  (company-lsp-enable-recompletion t))
    )

  ;; lsp ruby support
  (leaf lsp-ruby
    :ensure t
    ;; :hook ((ruby-mode-hook . lsp-ruby-enable))
    )

  ;; lsp C/C++ support
  (leaf ccls
    :ensure t
    :config
    (custom-set-variables `(ccls-executable ,(executable-find "ccls"))))

  ;; lsp java support
  (leaf lsp-java
    :ensure t
    :hook (java-mode-hook . lsp)))

(leaf ivy :ensute t
  :config
  (leaf counsel :ensure t
    :config
    (counsel-mode 1))
  (leaf swiper :ensure t)
  (ivy-mode 1))

(leaf helm
  :disabled t
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
    :custom ((helm-command-prefix-key . "C-c C-h")))
  
  (helm-autoresize-mode t)
  (helm-mode 1)

  ;; Change helm-command-prefix "C-x c" to "c-c h"
  ;; default "C-x c" is quite close to "C-x C-c" which quits Emacs
  ;;  (global-set-key (kbd "C-c h") helm-command-map)
  (global-unset-key (kbd "C-x c")))

(leaf elscreen
  :ensure t
  :require t
  :bind* (("C-c e k"       . elscreen-kill-screen-and-buffers)
          ;; confrict with org-mode
          ;; ("C-M-<right>" . elscreen-swap-next)
          ;; ("C-M-<left>"  . elscreen-swap-previous)
          ("C-<tab>"     . elscreen-next)
          ("C-S-<tab>"   . elscreen-previous)
          ("C-c e d"       . elscreen-dired)
          ("C-c e r"       . elscreen-screen-nickname))
  ;;  :init (el-get-bundle conao/elscreen-swap)
  :config
  (setq elscreen-tab-display-control nil)
  ;; (leaf session
  ;;   :init (el-get-bundle conao/revive)
  ;;   :requre t
  ;;   :config
  ;;   (setq session-initialize '(places session)
  ;;         session-globals-include '((kill-ring 100)
  ;;                                   (session-files-alist 500 t)
  ;;                                   (file-name-history 10000))
  ;;         session-globals-max-string 100000
  ;;         history-length t
  ;;         session-undo-check -1)
  ;;   (add-hook 'after-init-hook 'session-initialize))
  (leaf navbar
    :require t
    :init (el-get-bundle papaeye/emacs-navbar
                         :features (navbarx-elscreen navbarx-version navbarx-time))
    :config
    (setq navbar-item-list '(navbarx-version navbarx-time navbarx-elscreen))
    (navbar-mode)
    (display-time-mode)
    ;; (navbar-revive-workaround)
    )
  ;; (leaf elscreen-persist
  ;;   :require t
  ;;   :init (el-get-bundle robario/elscreen-persist)
  ;;   :config
  ;;   (elscreen-persist-mode 1)

  ;;   ;; desktop.el settings
  ;;   (setq desktop-files-not-to-save "")
  ;;   (setq desktop-restore-frames nil)
  ;;   (desktop-save-mode t))
  (leaf elscreen-server :disabled t)
  (custom-set-variables
   '(elscreen-prefix-key (kbd "C-c e"))
   '(elscreen-tab-display-kill-screen nil)    ;; don't show [x] mark in tab
   '(elscreen-tab-display-control nil))       ;; don't show [<->] mark in header-line
  (setq elscreen-display-screen-number nil)   ;; don't show screen number in mode-line
  (elscreen-start))

(leaf undo-tree :ensure t
  :config
  (leaf undohist :ensure t :require t
        :init
        (defalias 'user-setting-directory 'locate-user-emacs-file)
    :config
    (undohist-initialize)
    (setq undohist-directory (user-setting-directory "undohist")
          undohist-ignored-files '("/tmp" "/elpa" "/el-get")))
  (global-undo-tree-mode))

(leaf auto-complete
  :disabled t
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
           (ac-use-fuzzy . t)                  ; use fuzzy
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
