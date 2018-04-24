;;; 30_utility.el ---                                 -*- lexical-binding: t; -*-

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

;;;;;;;;;;;;;;;;;;
;; API's
(use-package dash :ensure t)
(use-package s :ensure t)
(use-package f :ensure t)

;;;;;;;;;;;;;;;;;;
;; git modes
(use-package magit              :ensure t :defer t :bind ("C-x v"   . magit-status))
(use-package gitconfig-mode     :ensure t :defer t)
(use-package gitignore-mode     :ensure t :defer t)
(use-package gitattributes-mode :ensure t :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; small utilities
(use-package minibuf-isearch    :ensure t :defer t)
(use-package open-junk-file     :ensure t :defer t :bind ("C-x C-x" . open-junk-file))
(use-package lispxmp            :ensure t :defer t :bind ("C-c C-e" . lispxmp))
(use-package htmlize            :ensure t :defer t)
(use-package ctable             :ensure t :defer t)
(use-package free-keys          :ensure t :defer t
  :commands (free-keys free-keys-set-prefix))

(use-package howm :ensure t :defer t
  ;; https://howm.osdn.jp/index-j.html
  )

(use-package mode-compile  :ensure t :defer t
  :bind* (("C-c c" . mode-compile))
  :config
  (use-package mode-compile-kill
    :bind* (("C-c k" . mode-compile-kill)))
  (setq mode-compile-always-save-buffer-p t
        mode-compile-never-edit-command-p t
        mode-compile-expert-p t
        mode-compile-reading-time 0))

(use-package rainbow-mode :ensure t :defer t :diminish (rainbow-mode . "")
  :commands rainbow-mode
  :hook (emacs-lisp-mode lisp-mode css-mode less-mode web-mode html-mode))

(use-package google-translate :ensure t :defer t
  :bind* (("C-x t"   . google-translate-at-point)
          ("C-x S-t" . google-translate-query-translate))
  :config  ;; 翻訳のデフォルト値を設定(ja -> en)（無効化は C-u する）
  (custom-set-variables
   '(google-translate-default-source-language "ja")
   '(google-translate-default-target-language "en")))

  ;; google-translate.elの翻訳バッファをポップアップで表示させる
  (push '("*Google Translate*") popwin:special-display-config)

(use-package elisp-slime-nav :ensure t :diminish (elisp-slime-nav-mode . "") :disabled t
  :hook (emacs-lisp-mode lisp-interaction-mode))

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
\\usepackage{physics}
\\newcommand{\\ee}{\\mathrm{e}}
\\newcommand{\\jj}{\\mathrm{j}}
\\newcommand{\\ii}{\\mathrm{i}}
\\newcommand{\\rot}{{\\nabla\\times}}
\\newcommand{\\up}{\\uparrow}
\\color{white}"
        latex-math-preview-initial-page-of-symbol-list '((math . nil) (text . nil)))
  (add-to-list 'latex-math-preview-command-option-alist
               '(gs-to-png "-q" "-dSAFER" "-dNOPAUSE" "-dBATCH" "-sDEVICE=pngalpha"
                           "-dEPSCrop" "-r600" "-dTextAlphaBits=4"
                           "-dGraphicsAlphaBits=4" "-dQUIET")))

(use-package clang-format :ensure t
  :if (executable-find "clang-format")
  :bind (("C-c f f" . clang-format-buffer)
         ("C-c f r" . clang-format-region)))
  ;; install clang-format
  ;; > brew update
  ;; > brew install clang-format)

(use-package shell-pop :ensure t :defer t
  :bind ("C-o" . shell-pop)
  :config
  ;; (setq shell-pop-shell-type (executable-find "fish")
  ;;       shell-pop-shell-type '("term" "*terminal<1>*" (lambda () (multi-term)))))
  )

(use-package multi-term :ensure t
  :config
  ;; (setq multi-term-program (executable-find "fish")))
  )

(use-package grep
  :bind (("M-s g" . grep))
  :config
  (grep-apply-setting 'grep-command "grep -inH ")
  
  (use-package wgrep :ensure t
    :bind (("M-s g" . grep))
    :config
    (setq wgrep-change-readonly-file t
          wgrep-enable-key "e")))

(use-package auto-async-byte-compile :ensure t :defer t :disabled t
  :config
  (setq auto-async-byte-compile-exclude-files-regexp "/junk/"
        eldoc-idle-delay 0.2
        eldoc-minor-mode-string "")  ;; dont show ElDoc in mode line
  (find-function-setup-keys))

;; el-get packages
(use-package other-window-or-split
  :init (el-get-bundle conao/other-window-or-split)
  :bind* (("C-t"   . ws-other-window-or-split)
          ("C-S-t" . ws-previous-other-window-or-split)
          ("M-t"   . ws-split-window-dwim)
          ("C-c j" . ws-adjust-windows-size)
          ("C-c u" . ws-window-resizer))
  :config
  (setq split-window-width-with-em 100))

(use-package point-undo
  :init (el-get-bundle emacswiki:point-undo)
  :bind* (("M-i p" . point-undo)
          ("M-i n" . point-redo)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; big utilities
(use-package auto-install :ensure t :defer t :disabled t
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
  :init
  (defvar org2blog-map nil "org2blog-prefix-map")
  (define-prefix-command 'org2blog-map)
  
  :bind (("C-c n n" . org2blog/wp-new-entry)
         :map org-mode-map
         ("C-c n" . org2blog-map)
         :map org2blog-map
         ("i" . org2blog/wp-login)
         ("o" . org2blog/wp-logout)
         ("p" . org2blog/wp-post-buffer-and-publish)
         ("d" . org2blog/wp-post-buffer)                   ;; post as draft
         ("D" . org2blog/wp-post-buffer-as-page)           ;; post as draft
         ("l" . org2blog/wp-insert-post-or-page-link))
  :config
  (org2blog/wp-reload-entry-mode-map)
  
  (use-package netrc
    :config
    (setq cotoday (netrc-machine (netrc-parse "~/.netrc") "conao3" t)))
  
  (setq org2blog/wp-show-post-in-browser     'show
        org2blog/wp-use-sourcecode-shortcode t
        org2blog/wp-blog-alist `(("today-note"
                                  :url "http://conao3.com/xmlrpc.php"
                                  :username ,(netrc-get cotoday "login")
                                  :password ,(netrc-get cotoday "password")
                                  ;; :wp-code t
                                  )))

  (prog1 "Setting templete with wp-new-entry"
    (setq org2blog/wp-buffer-template
          "#+DATE: %s
  #+OPTIONS: toc:t num:nil todo:nil pri:nil tags:nil ^:nil
  #+CATEGORY: %s
  #+TAGS: %s
  #+TITLE: %s
  #+PERMALINK: %s\n* 概要
  #+HTML: <!--more-->\n* 環境\n")
    
    (defun my-format-function (format-string)
      (format format-string
              (format-time-string "[%Y-%m-%d %a %H:%M]" (current-time))
              (read-string "input category: " "emacs")
              (read-string "input tags: " "emacs")
              (read-string "input title: ")
              (read-string "input permalink-keyword: ")))
    (setq org2blog/wp-buffer-format-function 'my-format-function))
  (prog1 ""
    (advice-add 'url-http-create-request :override
                'url-http-create-request-debug)
    (defun url-http-create-request-debug (&optional ref-url)
      "Create an HTTP request for <code>url-http-target-url', referred to by REF-URL."
      (let* ((extra-headers)
             (request nil)
             (no-cache (cdr-safe (assoc "Pragma" url-http-extra-headers)))
             (using-proxy url-http-proxy)
             (proxy-auth (if (or (cdr-safe (assoc "Proxy-Authorization"
                                                  url-http-extra-headers))
                                 (not using-proxy))
                             nil
                           (let ((url-basic-auth-storage
                                  'url-http-proxy-basic-auth-storage))
                             (url-get-authentication url-http-proxy nil 'any nil))))
             (real-fname (url-filename url-http-target-url))
             (host (url-http--encode-string (url-host url-http-target-url)))
             (auth (if (cdr-safe (assoc "Authorization" url-http-extra-headers))
                       nil
                     (url-get-authentication (or
                                              (and (boundp 'proxy-info)
                                                   proxy-info)
                                              url-http-target-url) nil 'any nil))))
        (if (equal "" real-fname)
            (setq real-fname "/"))
        (setq no-cache (and no-cache (string-match "no-cache" no-cache)))
        (if auth
            (setq auth (concat "Authorization: " auth "\r\n")))
        (if proxy-auth
            (setq proxy-auth (concat "Proxy-Authorization: " proxy-auth "\r\n")))
        
        ;; Protection against stupid values in the referrer
        (if (and ref-url (stringp ref-url) (or (string= ref-url "file:nil")
                                               (string= ref-url "")))
            (setq ref-url nil))
        
        ;; We do not want to expose the referrer if the user is paranoid.
        (if (or (memq url-privacy-level '(low high paranoid))
                (and (listp url-privacy-level)
                     (memq 'lastloc url-privacy-level)))
            (setq ref-url nil))
        
        ;; url-http-extra-headers contains an assoc-list of
        ;; header/value pairs that we need to put into the request.
        (setq extra-headers (mapconcat
                             (lambda (x)
                               (concat (car x) ": " (cdr x)))
                             url-http-extra-headers "\r\n"))
        (if (not (equal extra-headers ""))
            (setq extra-headers (concat extra-headers "\r\n")))
        
        ;; This was done with a call to </code>format'.  Concatenating parts has
        ;; the advantage of keeping the parts of each header together and
        ;; allows us to elide null lines directly, at the cost of making
        ;; the layout less clear.
        (setq request
              (concat
               ;; The request
               (or url-http-method "GET") " "
               (url-http--encode-string
                (if using-proxy (url-recreate-url url-http-target-url) real-fname))
               " HTTP/" url-http-version "\r\n"
               ;; Version of MIME we speak
               "MIME-Version: 1.0\r\n"
               ;; (maybe) Try to keep the connection open
               "Connection: " (if (or using-proxy
                                      (not url-http-attempt-keepalives))
                                  "close" "keep-alive") "\r\n"
                                  ;; HTTP extensions we support
                                  (if url-extensions-header
                                      (format
                                       "Extension: %s\r\n" url-extensions-header))
                                  ;; Who we want to talk to
                                  (if (/= (url-port url-http-target-url)
                                          (url-scheme-get-property
                                           (url-type url-http-target-url) 'default-port))
                                      (format
                                       "Host: %s:%d\r\n" host (url-port url-http-target-url))
                                    (format "Host: %s\r\n" host))
                                  ;; Who its from
                                  (if url-personal-mail-address
                                      (concat
                                       "From: " url-personal-mail-address "\r\n"))
                                  ;; Encodings we understand
                                  (if (or url-mime-encoding-string
                                          ;; MS-Windows loads zlib dynamically, so recheck
                                          ;; in case they made it available since
                                          ;; initialization in url-vars.el.
                                          (and (eq 'system-type 'windows-nt)
                                               (fboundp 'zlib-available-p)
                                               (zlib-available-p)
                                               (setq url-mime-encoding-string "gzip")))
                                      (concat
                                       "Accept-encoding: " url-mime-encoding-string "\r\n"))
                                  (if url-mime-charset-string
                                      (concat
                                       "Accept-charset: "
                                       (url-http--encode-string url-mime-charset-string)
                                       "\r\n"))
                                  ;; Languages we understand
                                  (if url-mime-language-string
                                      (concat
                                       "Accept-language: " url-mime-language-string "\r\n"))
                                  ;; Types we understand
                                  "Accept: " (or url-mime-accept-string "*/*") "\r\n"
                                  ;; User agent
                                  (url-http-user-agent-string)
                                  ;; Proxy Authorization
                                  proxy-auth
                                  ;; Authorization
                                  auth
                                  ;; Cookies
                                  (when (url-use-cookies url-http-target-url)
                                    (url-http--encode-string
                                     (url-cookie-generate-header-lines
                                      host real-fname
                                      (equal "https" (url-type url-http-target-url)))))
                                  ;; If-modified-since
                                  (if (and (not no-cache)
                                           (member url-http-method '("GET" nil)))
                                      (let ((tm (url-is-cached url-http-target-url)))
                                        (if tm
                                            (concat "If-modified-since: "
                                                    (url-get-normalized-date tm) "\r\n"))))
                                  ;; Whence we came
                                  (if ref-url (concat
                                               "Referer: " ref-url "\r\n"))
                                  extra-headers
                                  ;; Length of data
                                  (if url-http-data
                                      (concat
                                       "Content-length: " (number-to-string
                                                           (length url-http-data))
                                       "\r\n"))
                                  ;; End request
                                  "\r\n"
                                  ;; Any data
                                  url-http-data))
        ;; Bug#23750
        ;;(unless (= (string-bytes request)
        ;;           (length request))
        ;;  (message "   text byte %d vs %d length" (string-bytes request) (length request)))
        ;;(message "===============================")
        ;;(error "Multibyte text in HTTP request: %s" request))
        (url-http-debug "Request is: \n%s" request)
        request))))

(use-package pdf-tools :ensure t :defer t
  :config
  ;; depend on glib, poppler, ghostscript, imagemagick
  ;; $ brew install glib poppler ghostscript imagemagick
  (pdf-tools-install t)

  (add-to-list 'auto-mode-alist '("\\.pdf$" . 'pdf-view-mode))
  
  (prog1 "linum mode off in pdf-mode"
    (defcustom linum-disabled-modes-list '(doc-view-mode pdf-view-mode)
      "* List of modes disabled when global linum mode is on"
      :type '(repeat (sexp :tag "Major mode"))
      :tag " Major modes where linum is disabled: "
      :group 'linum)
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
        (linum-mode 1)))))

(use-package dired :defer t
  :config
  (setq dired-dwim-target t
        delete-by-moving-to-trash t)
  
  (use-package wdired :defer t
    :bind (:map dired-mode-map
                ("r" . wdired-change-to-wdired-mode)))
  (use-package dired-rainbow :ensure t :defer t)
  (use-package dired-filter  :ensure t :defer t
    :config
    (add-hook 'dired-mode-hook 'dired-filter-mode))
  (use-package dired-subtree :ensure t :defer t
    :bind (:map dired-mode-map
                ("<tab>"   . dired-subtree-remove)   ;; folding with tab
                ("C-x n n" . dired-subtree-narrow))  ;; narrowing subtree
    :init
    (use-package dired-details :ensure t
      :bind (:map dired-mode-map
                  ("i" . dired-subtree-insert))
      :config
      (setq dired-details-hidden-string ""
            dired-details-hide-link-targets nil))
    :config

    ;; ファイル名以外の情報を(と)で隠したり表示したり
    (dired-details-install)
    (setq dired-details-hidden-string     ""
          dired-details-hide-link-targets nil
          dired-details-initially-hide    nil)

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

(provide '30_utility)
;;; 30_utility.el ends here
