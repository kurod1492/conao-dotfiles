;;; 30_utility.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Naoya Yamashita

;; Author: Naoya Yamashita <conao@Naoya-MacBook-Air.local>
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; small utilities
(use-package free-keys          :ensure t :defer t)
(use-package minibuf-isearch    :ensure t :defer t)
(use-package open-junk-file     :ensure t :defer t :bind ("C-x C-x" . open-junk-file))
(use-package shell-pop          :ensure t :defer t :bind ("C-o"     . shell-pop))
(use-package lispxmp            :ensure t :defer t :bind ("C-c C-e" . lispxmp))
(use-package htmlize            :ensure t :defer t)

;;;;;;;;;
;; git modes
(use-package magit              :ensure t :defer t :bind ("C-x v"   . magit-status))
(use-package gitconfig-mode     :ensure t :defer t)
(use-package gitignore-mode     :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; big utilities
(use-package auto-install :ensure t :defer t
  :commands (auto-install-from-buffer
             auto-install-from-url
             auto-install-from-emacswiki
             auto-install-from-gist
             auto-install-mode)
  :config
  (setq auto-install-directory "~/.emacs.d/auto-install"
        auto-install-emacswiki-base-url "https://www.emacswiki.org/emacs/download/"
        auto-install-save-confirm nil
        auto-install-replace-confirm nil
        auto-install-install-confirm nil
        auto-install-from-dired-confirm nil))

(use-package org2blog :ensure t :defer t
  :init (setq org2blog/wp-keymap-prefix "C-c n")
  :bind (
         ;;("" . org2blog/wp-mode)
;;          :map org2blog/wp-entry-mode-map
;;          ("n" . org2blog/wp-new-entry)
;;          ("i" . org2blog/wp-login)
;;          ("o" . org2blog/wp-logout)
;;          ("p" . org2blog/wp-post-buffer-as-page-and-publish)
;;          ("d" . org2blog/wp-post-buffer)           ;; draft
;;          ("D" . org2blog/wp-post-buffer-as-page)   ;; draft
;;          ("l" . org2blog/wp-insert-post-or-page-link))
         ("C-c n n" . org2blog/wp-new-entry)
         :map org-mode-map
         ("C-c n i" . org2blog/wp-login)
         ("C-c n o" . org2blog/wp-logout)
         ("C-c n p" . org2blog/wp-post-buffer-and-publish)
         ("C-c n d" . org2blog/wp-post-buffer)           ;; draft
         ("C-c n D" . org2blog/wp-post-buffer-as-page)   ;; draft
         ("C-c n l" . org2blog/wp-insert-post-or-page-link))
  :config
  (setq org2blog/wp-buffer-template
        "#+DATE: %s
#+OPTIONS: toc:t num:nil todo:nil pri:nil tags:nil ^:nil
#+CATEGORY: %s
#+TAGS: %s
#+TITLE: %s
#+PERMALINK: %s
* 概要
#+HTML: <!--more-->
* 環境\n")
  
  (defun my-format-function (format-string)
    (format format-string
            (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
            (read-string "input category: " "emacs")
            (read-string "input tags: " "emacs")
            (read-string "input title: ")
            (read-string "input permalink-keyword: ")))
  
  (setq org2blog/wp-buffer-format-function 'my-format-function)

  (setq org2blog/wp-blog-alist
        '(("today-note"
           :url "http://conao3.com//xmlrpc.php"
           :username "conao"
           ;;:wp-code t
           ))))

(use-package pdf-tools :ensure t :defer t
  :config
  ;; depend on glib, poppler, ghostscript, imagemagick
  ;; $ brew install glib poppler ghostscript imagemagick
  (pdf-tools-install t)

  (add-to-list 'auto-mode-alist (cons "\\.pdf$" 'pdf-view-mode))
  ;; linum mode off in pdf-mode
  (defcustom linum-disabled-modes-list '(doc-view-mode pdf-view-mode)
    "* List of modes disabled when global linum mode is on"
    :type '(repeat (sexp :tag "Major mode"))
    :tag " Major modes where linum is disabled: "
    :group 'linum
    )
  (defcustom linum-disable-starred-buffers 't
    "* Disable buffers that have stars in them like *Gnu Emacs*"
    :type 'boolean
    :group 'linum)
  (defun linum-on ()
    "* When linum is running globally,
disable line number in modes defined in `linum-disabled-modes-list'.
Changed by linum-off.
Also turns off numbering in starred modes like *scratch*"
    (unless (or (minibufferp) (member major-mode linum-disabled-modes-list)
                (and linum-disable-starred-buffers (string-match "*" (buffer-name))))
      (linum-mode 1))))

(use-package auto-async-byte-compile :ensure t :defer t :disabled t
  :config
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/"
        eldoc-idle-delay 0.2
        eldoc-minor-mode-string "")  ;; dont show ElDoc in mode line
  (find-function-setup-keys))

(use-package dired :defer t
  :config
  (use-package dired-rainbow :ensure t :defer t)
  (use-package dired-filter  :ensure t :defer t
    :config
    (add-hook 'dired-mode-hook 'dired-filter-mode))
  (use-package dired-subtree :ensure t :defer t
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
    (define-key dired-mode-map (kbd "^") 'dired-subtree-up-dwim)))

(use-package mode-compile  :ensure t :defer t
  :bind* (("C-c c" . mode-compile))
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
        mode-compile-reading-time 0))

(use-package rainbow-mode :ensure t :defer t :diminish (rainbow-mode . "")
  :commands rainbow-mode
  :init
  (hook-into-modes #'rainbow-mode
                   'emacs-lisp-mode-hook
                   'lisp-mode-hook
                   'css-mode-hook
                   'less-mode-hook
                   'web-mode-hook
                   'html-mode-hook))

(use-package google-translate :ensure t :defer t
  :init
  (use-package popwin
    :defer t
    :ensure t
    :config (setq display-buffer-function      'popwin:display-buffer
                  popwin:popup-window-position 'bottom))
  :config  ;; 翻訳のデフォルト値を設定(ja -> en)（無効化は C-u する）
  (custom-set-variables
   '(google-translate-default-source-language "ja")
   '(google-translate-default-target-language "en"))

  ;; google-translate.elの翻訳バッファをポップアップで表示させる
  (push '("*Google Translate*") popwin:special-display-config)
  :bind* (("C-x t"   . google-translate-at-point)
          ("C-x S-t" . google-translate-query-translate)))

(use-package elisp-slime-nav :ensure t :diminish (elisp-slime-nav-mode . "")
  :config
  (hook-into-modes 'elisp-slime-nav-mode
                   'emacs-lisp-mode-hook
                   'lisp-interaction-mode-hook))

(use-package latex-math-preview :ensure t
  :if (executable-find "platex")
  :bind (("C-c l l" . latex-math-preview-expression)
         ("C-c l s" . latex-math-preview-insert-mathematical-symbol))
  :config
  (setq-default latex-math-preview-tex-to-png-for-preview '(platex dvips-to-eps gs-to-png)
                latex-math-preview-tex-to-png-for-save    '(platex dvipng)
                latex-math-preview-tex-to-eps-for-save    '(platex dvips-to-eps)
                latex-math-preview-tex-to-ps-for-save     '(platex dvips-to-ps)
                latex-math-preview-beamer-to-png          '(platex dvipdfmx gs-to-png))
  (setq latex-math-preview-latex-template-header
"\\documentclass{jsarticle}
\\pagestyle{empty}
\\usepackage[dvips]{color}
color{white}"
        latex-math-preview-initial-page-of-symbol-list '((math . nil) (text . nil)))
  (add-to-list 'latex-math-preview-command-option-alist
               '(gs-to-png "-q" "-dSAFER" "-dNOPAUSE" "-dBATCH" "-sDEVICE=pngalpha"
                           "-dEPSCrop" "-r600" "-dTextAlphaBits=4"
                           "-dGraphicsAlphaBits=4" "-dQUIET")))

;; el-get packages
(use-package other-window-or-split
  :init (el-get-bundle conao/other-window-or-split)
  :bind* ("C-t" . other-window-or-split))

(provide '30_utility)
;;; 30_utility.el ends here
