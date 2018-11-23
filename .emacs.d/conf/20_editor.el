;;; 20_editor.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@naoya-imac.local>
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

;;; Code:

(use-package helm :diminish ""
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
         :map helm-command-map
         ("o"       . helm-occur)
         :map helm-map
         ("<tab>"   . helm-execute-persistent-action)
         ("C-i"     . helm-execute-persistent-action)
         ("C-z"     . helm-select-action))
  :config
  (use-package helm-config)
  (use-package helm-descbinds :ensure t :defer t
    :config
    (helm-descbinds-install))
  
  ;; Change helm-command-prefix "C-x c" to "c-c h"
  ;; default "C-x c" is quite close to "C-x C-c" which quits Emacs
  ;;  (global-set-key (kbd "C-c h") helm-command-map)
  (global-unset-key (kbd "C-x c"))
  
  (when (executable-find "curl")
    (setq helm-net-prefer-curl t))
  
  (setq helm-split-window-inside-p            t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line nil)
  
  ;; helm window setting
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode t)
  
  ;; enable fuzzy seach in helm-mini, semantic
  (setq helm-M-x-fuzzy-match        t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-semantic-fuzzy-match   t
        helm-imenu-fuzzy-match      t
        helm-apropos-fuzzy-match    t)
  
  (setq helm-input-idle-delay 0.0) ; 文字列を入力してから検索するまでのタイムラグ。デフォルトで 0
  (setq helm-candidate-number-limit 100) ; 表示する最大候補数。デフォルトで 50

  ;; use man in helm (C-c h m)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  
  (helm-mode 1))

(use-package smartparens :diminish ""
  :init (use-package markdown-mode)
  :config
  (use-package smartparens-config :straight nil)
  (sp-pair "$" "$")

  (sp-use-smartparens-bindings)
  (smartparens-global-strict-mode t)
  (show-smartparens-global-mode t)
  (smartparens-global-mode t))

(use-package auto-complete :demand t :diminish ""
  :bind (:map ac-menu-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous))
  :init
  (use-package fuzzy  )
  (use-package pos-tip)
  
  :config
  (use-package auto-complete-config)
  (ac-config-default)
  (setq ac-auto-show-menu   0
        ac-delay            0
        ac-quick-help-delay 1
        ac-menu-height      15
        ac-auto-start       1
        ac-use-menu-map     t)
  (push 'ac-source-filename ac-sources)
  
  (ac-flyspell-workaround)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'yatex-mode)
  
  (push 'ac-source-filename ac-sources)
  ;; auto-complete の候補に日本語を含む単語が含まれないようにする
  ;; http://d.hatena.ne.jp/IMAKADO/20090813/1250130343
  (defadvice ac-word-candidates (after remove-word-contain-japanese activate)
    (let ((contain-japanese (lambda (s) (string-match (rx (category japanese)) s))))
      (setq ad-return-value
            (remove-if contain-japanese ad-return-value)))))

(use-package yasnippet
  :bind (("C-c i i" . yas-insert-snippet)
         ("C-c i n" . yas-new-snippet)
         ("C-c i e" . yas-visit-snippet-file)
         ("C-c i v" . yas-describe-tables))
  :config
  (use-package yatemplate ;; :defer t
    :init (use-package buttercup :defer t)
    :config
    (setq yatemplate-dir (user-setting-directory "template"))
    (yatemplate-fill-alist)
    (auto-insert-mode 1))
  
  (prog1 "yas-desable-flymake-when-expanding"
    (defvar flymake-is-active-flag nil)

    (defadvice yas-expand-snippet
        (before inhibit-flymake-syntax-checking-while-expanding-snippet activate)
      (setq flymake-is-active-flag
            (or flymake-is-active-flag
                (assoc-default 'flymake-mode (buffer-local-variables))))
      (when flymake-is-active-flag
        (flymake-mode-off)))

    (add-hook 'yas/after-exit-snippet-hook
              '(lambda ()
                 (when flymake-is-active-flag
                   (flymake-mode-on)
                   (setq flymake-is-active-flag nil)))))
  (yas-global-mode 1))

(use-package elscreen :demand t
  :init
  (el-get-bundle conao/elscreen)
  (el-get-bundle conao/revive)
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
  (use-package session
    :init (el-get-bundle emacsorphanage/session)
    :config
    (setq session-initialize '(places session)
          session-globals-include '((kill-ring 100)
                                    (session-files-alist 500 t)
                                    (file-name-history 10000))
          session-globals-max-string 100000
          history-length t
          session-undo-check -1)
    (add-hook 'after-init-hook 'session-initialize))
  (use-package navbar
    :init (el-get-bundle papaeye/emacs-navbar
            :features (navbarx-elscreen navbarx-version navbarx-time))
    :config
    (setq navbar-item-list '(navbarx-version navbarx-time navbarx-elscreen))
    (navbar-mode)
    (display-time-mode)
    (navbar-revive-workaround))
  (use-package elscreen-persist
    :init (el-get-bundle robario/elscreen-persist)
    :config
    (elscreen-persist-mode 1)

    ;; desktop.el settings
    (setq desktop-files-not-to-save "")
    (setq desktop-restore-frames nil)
    (desktop-save-mode t))
  (use-package elscreen-server :disabled)
  (custom-set-variables
   '(elscreen-prefix-key (kbd "C-c e"))
   '(elscreen-tab-display-kill-screen nil)    ;; don't show [x] mark in tab
   '(elscreen-tab-display-control nil))       ;; don't show [<->] mark in header-line
  (setq elscreen-display-screen-number nil)   ;; don't show screen number in mode-line
  (elscreen-start))

(use-package fold-dwim
  :bind (("<f7>"     . fold-dwim-toggle)
         ("M-<f7>"   . fold-dwim-hide-all)
         ("C-M-<f7>" . fold-dwim-show-all))
  :init
  (use-package hideshow
    :diminish (hs-minor-mode . "")
    :config
    (add-hook 'find-file-hook
              (lambda () ;;(hs-minor-mode 1)
                (unless (or (string-equal (file-name-extension buffer-file-name) "")
                            (string-equal (file-name-extension buffer-file-name) "txt")
                            (string-equal (file-name-extension buffer-file-name) "TXT")
                            (string-equal (file-name-extension buffer-file-name) "pdf")
                            (string-equal (file-name-extension buffer-file-name) "PDF"))
                                        ;(hs-minor-mode 1)
                  )))))

(use-package undo-tree :diminish ""
  :config
  (use-package undohist
    :config
    (undohist-initialize)
    (setq undohist-directory (user-setting-directory "undohist")
          undohist-ignored-files '("/tmp" "/elpa" "/el-get")))
  (global-undo-tree-mode))

(use-package flycheck
  :config
  (use-package flycheck-pos-tip)
  (setq flycheck-disabled-checkers '(emacs-lisp emacs-lisp-checkdoc))
  (global-flycheck-mode)
  (custom-set-variables
   '(flycheck-keymap-prefix (kbd "C-c f"))
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))
  (smartrep-define-key
      global-map "M-g" '(("M-n" . 'flymake-goto-next-error)
                         ("M-p" . 'flymake-goto-prev-error))))

(use-package sequential-command
  :config
  (use-package sequential-command-config)
  (sequential-command-setup-keys))

(use-package anzu :diminish "" :disabled t ;; C-s bind to helm-occur
  :init
  (use-package migemo
    :if (executable-find "cmigemo")
    :ensure t
    :config
    ;; depend on latest cmigemo
    ;; $ brew install cmigemo --HEAD
    (setq migemo-command "cmigemo")
    (setq migemo-options '("-q" "--emacs"))
    (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")
    
    (setq migemo-user-dictionary nil)
    (setq migemo-regex-dictionary nil)
    (setq migemo-coding-system 'utf-8-unix)
    (migemo-init))
  :config
  (global-anzu-mode 1)
  (when (executable-find "cmigemo")
    (setq anzu-use-migemo t))
  (setq anzu-search-threshold 1000))

(use-package selected :diminish (selected-minor-mode . "") :demand t
  :init
  (setq selected-org-mode-map (make-sparse-keymap))
  :bind (:map selected-keymap
              ;; capitalization
              ("u" . upcase-region)
              ("d" . downcase-region)
              ("c" . capitalize-region)
              
              ("k" . kill-region)
              ("m" . apply-macro-to-region-lines)

              ;; misc
              ("w" . count-words-region)
              ("q" . selected-off)
              :map selected-org-mode-map
              ("t" . org-table-convert-region))
  :config
  (selected-global-mode t))

(use-package multiple-cursors
  :init
  (use-package expand-region
    :bind* (("C-M-SPC" . mc/mark-all-dwim-or-expand-region))
    :config
    (defun mc/mark-all-dwim-or-expand-region (arg)
      (interactive "p")
      (cl-case arg
        (16 (mc/mark-all-dwim t))
        (4 (mc/mark-all-dwim nil))
        (1 (call-interactively 'er/expand-region))))))

(use-package which-key :diminish ""
  :config
  (which-key-setup-side-window-right)
  (which-key-mode t))

(use-package popwin
  :config
  (setq display-buffer-function      'popwin:display-buffer
        popwin:popup-window-position 'bottom)
  (popwin-mode 1)
  (push '("*magit*" :height 20) popwin:special-display-config)
  (push '("*magit*" :noselect t :height 40 :width 80 :stick t) popwin:special-display-config))

;;; el-get packages
(use-package auto-save-buffers-enhanced :demand t
  :init (el-get-bundle kentaro/auto-save-buffers-enhanced)
  :bind ("C-x a s" . auto-save-buffers-enhanced-toggle-activity)
  :config
  (auto-save-buffers-enhanced t)
  (setq auto-save-buffers-enhanced-interval        0.5
        auto-save-buffers-enhanced-exclude-regexps '("^/ssh:" "^/scp:" "^/rcync:" "\\.ignore$")))

(provide '20_editor)
;;; 20_editor.el ends here
