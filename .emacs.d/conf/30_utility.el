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

(use-package auto-install :ensure t :defer t
  :commands (auto-install-from-buffer
             auto-install-from-url
             auto-install-from-emacswiki
             auto-install-from-gist
             auto-install-mode)
  :config
  (setq auto-install-directory "~/.emacs.d/auto-install")
  (setq auto-install-emacswiki-base-url "https://www.emacswiki.org/emacs/download/")

  (setq auto-install-save-confirm nil)
  (setq auto-install-replace-confirm nil)
  (setq auto-install-install-confirm nil)
  (setq auto-install-from-dired-confirm nil)
  ;; always connect Internet to update package name when Emacs start up
  ;; but not necessary, auto-install will automatically update before install method
  ;; (auto-install-update-emacswiki-package-name t)

;;   (use-package split-root
;;     :init (auto-install-from-url "http://nschum.de/src/emacs/split-root/split-root.el")
;;     :config
;;     (setq split-root-window-height 20)
;;     (defun display-buffer-function--split-root (buf &optional ignore)
;;       (let ((window (split-root-window split-root-window-height)))
;;         (set-window-buffer window buf)
;;         window))
;;     (setq anything-display-function 'display-buffer-function--split-root))
  )

(use-package free-keys :ensure t :defer t)

(use-package fold-dwim :ensure t :defer t
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
                            (string-equal (file-name-extension buffer-file-name) "pdf")
                            (string-equal (file-name-extension buffer-file-name) "PDF"))
                  (hs-minor-mode 1))
                ))))

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

* 概要
#+HTML: <!--more-->
* 環境\n")
  
  (defun my-format-function (format-string)
    (format format-string
            (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
            (read-string "input category: " "emacs")
            (read-string "input tags: " "emacs")
            (read-string "input title: ")))
  
  (setq org2blog/wp-buffer-format-function 'my-format-function)

  (setq org2blog/wp-blog-alist
        '(("today-note"
           :url "http://conao3.com//xmlrpc.php"
           :username "conao"
           ;;:wp-code t
           ))))

(use-package pdf-tools :ensure t
  :config
  ;; depend on glib, poppler, ghostscript, imagemagick
  ;; brew install glib poppler ghostscript imagemagick
  (pdf-tools-install)

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
      (linum-mode 1)))
  )

;; el-get packages
(use-package other-window-or-split
  :init (el-get-bundle conao/other-window-or-split)
  :bind* ("C-t" . other-window-or-split))

(provide '30_utility)
;;; 30_utility.el ends here
