;;; 00_config.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Naoya Yamashita

;; Author: Naoya Yamashita <conao@naoya-imac.local>
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


(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

(use-package key-chord
  :ensure t
  :config
  (key-chord-mode 1))

(use-package use-package-chords
  :ensure t)

(use-package smartrep
  :ensure t)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))

(when (< emacs-major-version 23)
  (defvar user-emacs-directory "~/.emacs.d/"))


(progn ;; core-emacs
  (defun add-to-load-path (&rest paths)
    (let (path)
      (dolist (path paths paths)
        (let ((default-directory
                (expand-file-name (concat user-emacs-directory path))))
          (add-to-list 'load-path default-directory)
          (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
              (normal-top-level-add-subdirs-to-load-path))))))
  (add-to-load-path "site-lisp" "conf" "elpa")

  ;; coding system
  (set-language-environment "Japanese")
  (set-default-coding-systems   'utf-8-unix)
  (prefer-coding-system         'utf-8-unix)
  (set-file-name-coding-system  'utf-8-unix)
  (set-keyboard-coding-system   'utf-8-unix)
  (setq locale-coding-system    'utf-8-unix)
  (setq default-process-coding-system '(undecided-dos . utf-8-unix))

  ;; don't create file like *.~
  (setq make-backup-files nil)
  ;; don't create file like .#*
  (setq auto-save-default nil)
  ;; don't create backup file
  (setq vc-make-backup-files nil)
  ;; don't create .saves-Macbook-Air.local~
  (setq auto-save-list-file-prefix nil)
  ;; don't create .#aaa.txt@ -> conao@Macbook-Air.local
  (setq create-lockfiles nil)

  ;; don't show splash screen
  (setq inhibit-splash-screen t)

  ;; question from emacs
  (fset 'yes-or-no-p 'y-or-n-p)

  ;; reload file after renew
  (global-auto-revert-mode 1)

  ;; kill whole line
  (setq kill-whole-line t)

  ;; scroll-step
  (setq scroll-step 1)

  ;; stop beep and flash display
  (setq ring-bell-function 'ignore)

  ;; when occurs garbage collection
  (setq gc-cons-threshold (* 128 1024 1024))

  ;; 右から左に読む言語に対応させないことで描画高速化
  (setq-default bidi-display-reordering nil)

  ;; IME off when focus minibuffer
  (mac-auto-ascii-mode t)
  )

(progn ;; frame
  ;; frame title
(setq frame-title-format
      '("emacs " emacs-version (buffer-file-name " - %f")))

;;; window setting
;; hide toolbar, scroll-bar
(tool-bar-mode 0)
(scroll-bar-mode 0)

;;; mode-line setting
;; show line number
(line-number-mode t)
;; show column number
(column-number-mode t)
;; show battery force
(display-battery-mode t)

;; display line and char count
(defun count-lines-and-chars ()
  (if mark-active
      (format "%d lines, %d chars "
              (count-lines (region-beginning) (region-end))
              (- (region-end) (region-beginning)))))
(add-to-list 'default-mode-line-format
             '(:eval (count-lines-and-chars)))
)

(progn ;; window
  ;; show parent
(setq show-paren-delay 0)
(show-paren-mode t)

;; truncate
(setq-default truncate-lines t)

;; enlighten editing line
(global-hl-line-mode t)

;;; cursor
;; cursor not blink
(blink-cursor-mode 0)

;; hi-light region
(transient-mark-mode)
(setq highlight-nonselected-windows t)

;; temporary hi-light after yank region
(when window-system
  (defadvice yank (after ys:yank-highlight activate)
    (let ((ol (make-overlay (mark t) (point))))
      (overlay-put ol 'face 'highlight)
      (sit-for 1.0)
      (delete-overlay ol)))
  (defadvice yank-pop (after ys:yank-pop-highlight activate)
    (when (eq last-command 'yank)
      (let ((ol (make-overlay (mark t) (point))))
        (overlay-put ol 'face 'highlight)
        (sit-for 1.0)
        (delete-overlay ol)))))

;; open with drag file
(define-key global-map [ns-drag-file] 'ns-find-file)

;; save buffer condition
(desktop-save-mode t)
)

(progn ;;buffer
  ;; insert "\", instead "¥"
(define-key global-map [?¥] [?\\])

;; tab width
(setq-default tab-width 4)
(setq-default c-basic-offset 4)
(setq-default indent-tabs-mode nil)

;; delete region, when yank
(delete-selection-mode t)

;; comment style
(setq comment-style 'multi-line)
)

(progn ;; shortcut
  (bind-keys ("C-c a"   . align)
           ("C-c S-a" . align-regexp)
           ("C-c d"   . delete-trailing-whitespace)
           ("C-c b"   . battery)
           ("C-x e"   . eval-last-sexp)
           ("M-r"     . query-replace)
           ("M-c"     . c-mode))

(define-key key-translation-map (kbd "C-h") (kbd "<DEL>"))

;; C-x r f r : save frame configuration
;; C-x r j r : restore frame configuration

;; Cmd-Ctl-d ; open apple's default dicitonaly
)

(use-package generic-x
  :config
  (global-font-lock-mode))

(use-package time-stamp
  ;; %:a -- Monday 曜日
  ;; %#A -- MONDAY 全部大文字で曜日
  ;; %:b -- January 月

  ;; 桁数を指定すると指定した文字だけが表示される.
  ;; "%2#A"なら MO など．

  ;; %02H -- 15  時刻 (24 時間)
  ;; %02I -- 03  時刻 (12 時間)
  ;; %#p  -- pm  PM と AM の別
  ;; %P   -- PM  PM と AM の別
  ;; %w   -- 土曜なら 6. 日曜を 0 とし，何番目の曜日なのか
  ;; %02y -- 03  西暦の下 2 桁．
  ;; %z   -- jst  タイムゾーン
  ;; %Z   -- JST  タイムゾーン
  ;; %%   -- %自体を入力
  ;; %f   -- ファイル名
  ;; %F   -- ファイル名のフルパス
  ;; %s   -- マシン名
  ;; %u   -- ログインしたユーザ名
  ;; %U   -- ログインしたユーザのフルネーム
  ;; :defer t
  :config
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t
        time-stamp-start "[lL]ast[ -][uU]pdated[ \t]*:[ \t]*<"
        time-stamp-format "%:y/%02m/%02d"
        time-stamp-end ">"
        time-stamp-line-limit 20))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package linum
  ;; :defer t
  :config
  (global-linum-mode t)
  (setq linum-delay nil
        linum-format "%5d")
  (set-face-attribute 'linum nil :height 120))

(use-package newcomment
  :config
  (setq-default transient-mark-mode t)
  (setq comment-style 'multiline))

(use-package Flyspell
  :bind* (("<f12>" . flyspell-mode)
          ("<f10>" . flyspell-buffer)
          ("<f9>"  . ispell-word))
  :config
  (setq-default ispell-program-name "aspell")

  ;; fly-spell
  (mapc
   (lambda (hook)
     (add-hook hook 'flyspell-prog-mode))
   '(
     c++-mode-hook
     emacs-lisp-mode-hook
     ruby-mode-hook
     python-mode-hook
     ))
  (mapc
   (lambda (hook)
     (add-hook hook
               '(lambda () (flyspell-mode 1))))
   '(
     fundamental-mode
     text-mode-hook
     org-mode-hook
     yatex-mode-hook
     ))
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package wdired
  :config
  (setq delete-by-moving-to-trash t)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)

  ;; o . open dired
  )

(use-package view
  :config
  (setcar (cdr (assq 'view-mode minor-mode-alist))
          (if (fboundp 'propertize)
              (list (propertize " View"
                                'face '(:foreground "white"
                                                    :background "DeepPink1")))
            " View"))

  ;; viewmodeのキーバインドを優先
  (add-hook 'view-mode-hook
            '(lambda ()
               (setq minor-mode-Map-Alist
                     (delete (assq 'view-mode minor-mode-map-alist)
                             minor-mode-map-alist)
                     minor-mode-map-alist
                     (cons (cons 'view-mode view-mode-map) minor-mode-map-alist)))))

(use-package autoinsert
  :config
  (setq ;; auto-insert-query nil
   ;; auto-insert-alist nil
   auto-insert-directory "~/.emacs.d/template/")
  (auto-insert-mode 1))

(use-package outline
  :config
  (bind-key "<tab>" 'org-cycle outline-minor-mode-map)
  (bind-key "C-<tab>" 'org-global-cycle outline-minor-mode-map)
  (bind-key "C-c C-f" 'outline-forward-same-level outline-minor-mode-map)
  (bind-key "C-c C-b" 'outline-backward-same-level outline-minor-mode-map)
  (bind-key "C-c C-n" 'outline-next-visible-heading outline-minor-mode-map)
  (bind-key "C-c C-p" 'outline-previous-visible-heading outline-minor-mode-map)
  (bind-key "<tab>" 'org-cycle outline-mode-map)
  (bind-key "S-<tab>" 'org-global-cycle outline-mode-map))

(use-package helm :ensure t
  :config
  (require 'helm-config)
  (helm-mode 1)
  (define-key global-map (kbd "M-x")     'helm-M-x)
  (define-key global-map (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map (kbd "C-x C-r") 'helm-recentf)
  (define-key global-map (kbd "M-y")     'helm-show-kill-ring)
  (define-key global-map (kbd "C-c i")   'helm-imenu)
  (define-key global-map (kbd "C-x b")   'helm-buffers-list)
  (define-key global-map (kbd "M-r")     'helm-resume)
  (define-key global-map (kbd "C-M-h")   'helm-apropos)
  (define-key helm-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "C-h") 'delete-backward-char)
  (define-key helm-find-files-map (kbd "TAB") 'helm-execute-persistent-action)
  (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action))

(use-package auto-complete-config
  ;; :ensure t ;contain auto-complete
  :diminish (auto-complete-mode . "AC")
  :init
  (use-package pos-tip       :ensure t)
  (use-package fuzzy         :ensure t)
  (use-package auto-complete :ensure t)

  :config
  (ac-config-default)
  (global-auto-complete-mode t)
  (ac-flyspell-workaround)
  (setq ac-auto-start 1
        ac-delay 0.0
        ;; ac-use-menu-map t
        ac-use-fuzzy t
        ac-ignore-case 't
        ac-dwim t))

(use-package undohist
  :ensure t

  :config
  (undohist-initialize)
  (setq undohist-ignored-files '("/tmp" "/elpa"))
        undohist-directory "~/.emacs.d/undohist")

(use-package undo-tree
  :ensure t
  :diminish (undo-tree-mode . "UT")

  :config
  (global-undo-tree-mode)
  (bind-keys
   ("C-x u" . undo-tree-visualize)))

(use-package flycheck
  :ensure t

  :config
  (global-flycheck-mode)
  (bind-keys
   ("C-c n" . flycheck-next-error)
   ("C-c p" . flycheck-previous-error)
   ("C-c l" . flycheck-list-errors))

  (use-package flycheck-pos-tip :ensure t)
  (custom-set-variables
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages))

  ;; add js checker
  (flycheck-add-next-checker 'javascript-jshint
                             'javascript-gjslint))
(use-package fold-dwim :ensure t
  :bind*
  ("<f7>" . fold-dwim-toggle)
  ("C-<f7>" . fold-dwim-show-all)
  ("C-S-<f7>" . fold-dwim-hide-all)
  :config
  (use-package hideshow))

(use-package elscreen
  :ensure t

  :config
  (setq elscreen-prefix-key "\C-z"
        ;; don't show [x] mark in tab
        elscreen-tab-display-kill-screen nil
        ;; don't show [<->] mark in header-line
        elscreen-tab-display-control nil
        ;; don't show screen number in mode-line
        elscreen-display-screen-number nil)
  (bind-keys* ("C-z k"         . elscreen-kill-screen-and-buffers)
              ;; confrict with org-mode
              ;; ("C-M-<right>" . elscreen-swap-next)
              ;; ("C-M-<left>"  . elscreen-swap-previous)
              ("C-<tab>"     . elscreen-next)
              ("C-S-<tab>"   . elscreen-previous)
              ("C-z d"       . elscreen-dired)
              ("C-z r"       . elscreen-screen-nickname)))
  
;; use my lisp/elscreen-swap.el
(use-package elscreen-swap
  :bind* (;; confrict with org-mode
          ;; ("C-M-S-<right>" . elscreen-swap-next)
          ;; ("C-M-S-<left>"  . elscreen-swap-previous)
          ))

(use-package elscreen-persist
  :ensure t

  :config (elscreen-persist-mode))

(use-package elscreen-dired  :defer t)
(use-package elscreen-w3m    :defer t)
(use-package elscreen-server :defer t)
(use-package yatemplate
  :ensure t
  ;; :defer t
  :config
  (yatemplate-fill-alist)
  (auto-insert-mode 1))

(use-package free-keys :ensure t :defer t)
(use-package shell-pop :ensure t :defer t :bind ("C-o" . shell-pop))
(use-package magit     :ensure t :defer t :bind ("C-x v" . magit-status))
(use-package auto-install
  :ensure t
  :defer  t
  :commands
  auto-install-batch
  auto-install-from-url
  auto-install-from-gist
  auto-install-from-library
  auto-install-from-emacswiki
  :config (setq auto-install-directory "~/.emacs.d/site-lisp"))

(use-package tab-jump-out
  :ensure t
  :config (setq yas-fallback-behavior '(apply tab-jump-out 1)))

(use-package w3m :if (executable-find "w3m") :ensure t :defer t)
(use-package open-junk-file :ensure t :defer t :bind ("C-x C-x" . open-junk-file))
(use-package lispxmp        :ensure t :defer t :bind ("C-c C-e" . lispxmp))
(use-package paredit        :disabled t :ensure t :defer t)
(use-package auto-async-byte-compile
  :ensure t
  :defer  t
  :config
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/"
        eldoc-idle-delay 0.2
        eldoc-minor-mode-string "")  ;; dont show ElDoc in mode line
  (find-function-setup-keys))

(use-package minibuf-isearch :ensure t :defer  t)
(use-package sequential-command-config
  ;; :disabled t
  :init   (use-package sequential-command :ensure t)
  
  :config (global-set-key "\C-a" 'seq-home)
  (global-set-key "\C-e" 'seq-end)
  (global-set-key "\M-u" 'seq-upcase-backward-word)
  (global-set-key "\M-c" 'seq-capitalize-backward-word)
  (global-set-key "\M-l" 'seq-downcase-backward-word))

(use-package smartparens-config
  :init
  (use-package smartparens :ensure t)
  ;; :defer t
  :config
  (smartparens-global-mode)
  (sp-pair "$" "$"))

(use-package dired-rainbow :ensure t :defer t)
(use-package mode-compile
  :ensure t
  :defer  t
  :config
  (use-package mode-compile-kill
    :bind* (("C-c k" . mode-compile-kill)))
  ;; 全てバッファを自動的にセーブする
  (setq mode-compile-always-save-buffer-p t
        ;; コマンドをいちいち確認しない
        mode-compile-never-edit-command-p t
        ;; メッセージ出力を抑制
        mode-compile-expert-p t
        ;; メッセージを読み終わるまで待つ時間
        mode-compile-reading-time 0)
  :bind* (("C-c c" . mode-compile)))

(use-package auto-async-byte-compile
  :ensure t
  :defer  t
  :commands enable-auto-async-byte-compile-mode
  :init   (hook-into-modes #'enable-auto-async-byte-compile-mode
                           'emacs-lisp-mode-hook)
  :config
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/"))
(use-package session
  :ensure t
  ;; :defer  t
  :config
  (setq session-initialize '(de-saveplace session keys menus places)
        session-globals-include '((kill-ring 50)
                                  (session-files-alist 500 t)
                                  (file-name-history 10000))
        session-globals-maxlstring 100000000
        history-length t
        session-undo-check -1)
  (add-hook 'after-init-hook 'session-initialize))
(use-package electric-operator :disabled t :ensure t :defer t
  :commands electric-operator-mode
  :init
  (hook-into-modes #'electric-operator-mode
                   'c-mode-common-hook))

(use-package rainbow-mode :ensure t :defer t
  :commands rainbow-mode
  :init
  (hook-into-modes #'rainbow-mode
                   'emacs-lisp-mode-hook
                   'lisp-mode-hook
                   'css-mode-hook
                   'less-mode-hook
                   'web-mode-hook
                   'html-mode-hook))

(use-package dired-filter :ensure t :defer t
  :commands dired-filter-mode
  :init
  (add-hook 'dired-mode-hook 'dired-filter-mode))

(use-package dired-subtree :ensure t :defer t
  :commands dired-subtree-insert
  :init
  (use-package dired-details :ensure t)
  (bind-keys :map dired-mode-map
             ("i" . dired-subtree-insert))
  :config
  ;; org-modeのようにTABで折り畳む
  (define-key dired-mode-map (kbd "<tab>") 'dired-subtree-remove)
  ;; C-x n nでsubtreeにナローイング
  (define-key dired-mode-map (kbd "C-x n n") 'dired-subtree-narrow)

  ;; ファイル名以外の情報を(と)で隠したり表示したり
  (dired-details-install)
  (setq dired-details-hidden-string "")
  (setq dired-details-hide-link-targets nil)
  (setq dired-details-initially-hide nil)

  ;; dired-subtreeをdired-detailsに対応させる
  (defun dired-subtree-after-insert-hook--dired-details ()
    (dired-details-delete-overlays)
    (dired-details-activate))
  (add-hook 'dired-subtree-after-insert-hook
            'dired-subtree-after-insert-hook--dired-details)

  ;; find-dired対応
  (defadvice find-dired-sentinel (after dired-details (proc state) activate)
    (ignore-errors
      (with-current-buffer (process-buffer proc)
        (dired-details-activate))))
  ;; (progn (ad-disable-advice 'find-dired-sentinel 'after 'dired-details) (ad-update 'find-dired-sentinel))

  ;; [2014-12-30 Tue]^をdired-subtreeに対応させる
  (defun dired-subtree-up-dwim (&optional arg)
    "subtreeの親ディレクトリに移動。そうでなければ親ディレクトリを開く(^の挙動)。"
    (interactive "p")
    (or (dired-subtree-up arg)
        (dired-up-directory)))
  (define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim))

(use-package qiita :disabled t :ensure t :defer t
  :config (progn
            (setq qiita->token "xxxxxxxxxxx")))

(use-package google-translate
  :init
  (use-package popwin
    :defer t
    :ensure t
    :config (setq display-buffer-function      'popwin:display-buffer
                  popwin:popup-window-position 'bottom))
  :defer t
  :ensure t
  :config  ;; 翻訳のデフォルト値を設定(ja -> en)（無効化は C-u する）
  (custom-set-variables
   '(google-translate-default-source-language "ja")
   '(google-translate-default-target-language "en"))

  ;; google-translate.elの翻訳バッファをポップアップで表示させる
  (push '("*Google Translate*") popwin:special-display-config)
  :bind* (("C-x t"   . google-translate-at-point)
          ("C-x S-t" . google-translate-query-translate)))

(use-package codic :defer t :ensure t)
(use-package org2blog :defer t :ensure t
  :config
  (setq org2blog/wp-blog-alist '(("Conao-Tech"
                                  :url "http://conao.php.xdomain.jp/xmlrpc.php"  ;;xmlrcp.phpのURL
                                  :username "conao"
                                  :default-title "Hello World" ;; デフォルトタイトル
                                  :default-categories ("dairy") ;;カテゴリを指定
                                  :tags-as-categories nil ;; タグを指定
                                  ))
        org2blog/wp-buffer-template "#+DATE: %s
#+OPTIONS: toc:t ^:nil
#+CATEGORY: %s
#+TAGS:
#+DESCRIPTION:
#+TITLE: %s\n\n"))

(use-package htmlize :defer t :ensure t)

(use-package vbasense :defer t :ensure t
  :config

  ;; Keybinding
  (setq vbasense-popup-help-key "C-:")
  (setq vbasense-jump-to-definition-key "C->")

  ;; Make config suit for you. About the config item, eval the following sexp.
  ;;(customize-group "vbasense")

  ;; Do setting a recommemded configuration
  (vbasense-config-default))

(use-package web-mode
  :ensure t
  :defer  t
  :mode  (("\\.html?\\'" . web-mode)
          ("\\.jsp\\'"   . web-mode)
          ("\\.gsp\\'"   . web-mode)
          ("\\.php\\'"   . web-mode))
  :config (setq web-mode-markup-indent-offset 4
                web-mode-css-indent-offset    4
                web-mode-code-indent-offset   4
                indent-tabs-mode              nil))

(use-package yatex
  :ensure t
  :defer  t
  :mode (("\\.tex" . yatex-mode)))

(use-package org
  :ensure t
  ;;   :defer  t
  :config
  (require 'org-install)
  (setq org-html-htmlize-output-type 'css)
  (setq org-src-fontify-natively t)
  (setq org-latex-default-class "org-jsarticle")
  (add-to-list 'org-latex-classes
               '("org-jsarticle" "\\documentclass{jsarticle}
\\usepackage[top=2truecm, bottom=2truecm, left=1.5truecm, right=1.5truecm, includefoot]{geometry}
[NO-PACKAGES]
[NO-DEFAULT-PACKAGES]
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\rhead{\\thepage{}}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
                 (require 'ox-latex)
;; (require 'org-macro)
;; (require 'org-element)
                 (require 'ox-bibtex)

                 ;; LaTeX 形式のファイル PDF に変換するためのコマンド
                 (setq org-latex-pdf-process
                       '("platex %f"
                         "platex %f"
                         "bibtex %b"
                         "platex %f"
                         "platex %f"
                         "dvipdfmx %b.dvi"))

                 ;; \hypersetup{...} を出力しない
                 (setq org-latex-with-hyperref nil)
;; (require ‘ess)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)
;;    )
;;  )
;; (require 'org-babel-init)
;; (require 'org-babel-R)
;; (org-babel-load-library-of-babel)
;; (add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
;; R-mode を起動する時に ess-site をロード
;; (autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
;; R を起動する時に ess-site をロード
;; (autoload 'R "ess-site" "start R" t)

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images))

(use-package auto-save-buffers
  :config (progn
            (run-with-idle-timer 0.5 t 'auto-save-buffers)
            (bind-key "C-x a s" 'auto-save-buffers-toggle)))

(use-package navbarx-elscreen)
(use-package navbarx-version)
(use-package navbarx-time
  :config (progn
            (setq navbar-item-list '(navbarx-version
                                     navbarx-time
                                     navbarx-elscreen))
            (navbar-mode)
            (navbar-revive-workaround)
            (display-time-mode)

            ;;: elscreen-start ;;;
            (elscreen-start)))

(use-package ox-qmd
  :config (progn
            (add-to-list 'ox-qmd-language-keyword-alist '("shell-script" . "sh"))))

(use-package edit-list)

(use-package swap
  ;; swap-rectangle

  ;; とある二つの rectangle をいれかえます.
  ;; たとえば
  ;; A I X
  ;; B J Y
  ;; C K Z
  ;; という状態から,
  ;; X I A
  ;; Y J B
  ;; Z K C
  ;; みたいに変更したい場合, レジスタを使ったり kill-rectangle を使ったりする方法だと ちょっと余計な手間がかかりますよね.
  ;; この程度のことを一発でできる elisp くらい既にありそうなもんですが, 私の調べた限りでは見付からなかったのでした.
  ;; 結構こういう処理をしたいことが多かったので, 作ってみました.
  ;; 使い方は,
  ;; M-x swap-rectangle
  ;; いれかえたい片方の rectangle の開始位置で C-SPC
  ;; いれかえたい片方の rectangle の終了位置で Enter(ここまでがハイライトされます)
  ;; いれかえたいもう一方の rectangle の開始位置で C-SPC
  ;; いれかえたいもう一方の rectangle の終了位置で Enter
  ;; とするだけです.
  ;; やめるときは C-g で.
  ;; たぶんやってみればすぐわかるかと.

  ;; swap-region

  ;; 二つの領域をいれかえます.
  ;; まあ emacs なんだから yank-pop 使えばちょっとの手間でできますけど, せっかくだか ら region をいれかえるのも作ってみたわけです.
  ;; 使い方は,
  ;; M-x swap-region
  ;; いれかえたい片方の region の開始位置で C-SPC
  ;; いれかえたい片方の region の終了位置で Enter
  ;; いれかえたいもう一方の region の開始位置で C-SPC
  ;; いれかえたいもう一方の region の終了位置で Enter
  ;; とするだけです.
  ;; やめるときは C-g で.
  ;; たぶんやってみればすぐわかるかと.

  :commands swap-region swap-rectangle)
(use-package visual-basic-mode
  :config
  (setq visual-basic-mode-indent 4))

(use-package multiple-cursors
  :config
  (use-package smartrep
    :config (declare-function smartrep-define-key "smartrep"))

  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

  (global-unset-key "\C-t")

  (smartrep-define-key global-map "C-t"
    '(("C-t"      . 'mc/mark-next-like-this)
      ("n"        . 'mc/mark-next-like-this)
      ("p"        . 'mc/mark-previous-like-this)
      ("m"        . 'mc/mark-more-like-this-extended)
      ("u"        . 'mc/unmark-next-like-this)
      ("U"        . 'mc/unmark-previous-like-this)
      ("s"        . 'mc/skip-to-next-like-this)
      ("S"        . 'mc/skip-to-previous-like-this)
      ("*"        . 'mc/mark-all-like-this)
      ("d"        . 'mc/mark-all-like-this-dwim)
      ("i"        . 'mc/insert-numbers)
      ("o"        . 'mc/sort-regions)
      ("O"        . 'mc/reverse-regions))))



(provide '00_config)
;;; 00_config.el ends here
