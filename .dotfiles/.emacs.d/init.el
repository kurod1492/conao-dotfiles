;;; init.el ---                                      -*- lexical-binding: t; -*-

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

;;; Code:


(prog1 "Change user-emacs-directory"
  ;; enable debug
  (setq debug-on-error  t
        init-file-debug t)

  ;; you can run like 'emacs -q -l ~/hoge/init.el'
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name))))

  ;; change user-emacs-directory
  (setq user-emacs-directory
        (expand-file-name
         (format "local/%s.%s/"
                 emacs-major-version emacs-minor-version)
         user-emacs-directory))
  (make-directory user-emacs-directory t))

(prog1 "Load leaf.el"
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf.el"))
  (require 'leaf)
  (leaf leaf
    :doc "Symplify your init.el configuration"
    :doc "Initialize leaf dependent packages"
    :custom ((leaf-backend-ensure . 'package)
             (leaf-backend-bind   . 'bind-key))
    :config
    (leaf package
      :custom ((package-archives . '(("org"   . "https://orgmode.org/elpa/")
                                     ("melpa" . "https://melpa.org/packages/")
                                     ("gnu"   . "https://elpa.gnu.org/packages/"))))
      :config
      (package-initialize))
    (leaf bind-key :ensure t)))


(leaf initialize-emacs
  :config
  (leaf exec-path-from-shell
    :ensure t
    :when (memq window-system '(mac ns x))
    :custom ((exec-path-from-shell-check-startup-files . nil)
             (exec-path-from-shell-variables . '("PATH" "GOPATH")))
    :config
    (exec-path-from-shell-initialize))

  (global-unset-key (kbd "M-o"))
  (global-unset-key (kbd "M-t")))


(leaf conao3-packages
  :doc "Elisp packages are developed by conao3"
  :config
  (leaf melpa-packages
    :config
    (leaf seml-mode
      :ensure t
      :custom ((seml-live-refresh-interval     . 0.35)
               (seml-live-refresh-url-variable . ":type/:var1/:var2")
               (seml-live-refresh-url-quety    . '(targetpath targetfile)))))

  (leaf site-lisp-packages
    :config
    (leaf cort-test
      :load-path `,(locate-user-emacs-file "site-lisp/cort-test.el")
      :require t)

    (leaf feather
      :load-path `,(locate-user-emacs-file "site-lisp/feather.el")
      :require t)

    (leaf feather-server
      :load-path `,(locate-user-emacs-file "site-lisp/feather-server.el")
      :require t)

    (leaf leaf-browser
      :load-path `,(locate-user-emacs-file "site-lisp/leaf-browser.el")
      :require t
      :custom ((lbrowser-root-dir . "~/.emacs.d/site-lisp/leaf-browser.el/")
               (lbrowser-debugp   . t))
      :config
      (leaf htmlize :ensure t)
      (leaf simple-httpd
        :ensure t
        :custom ((httpd-show-backtrace-when-error . t))))

    (leaf navbar
      :load-path `,(locate-user-emacs-file "site-lisp/navbar.el")
      :require t)

    (leaf orglyth
      :load-path `,(locate-user-emacs-file "site-lisp/orglyth.el")
      :require t orglyth-html orglyth-latex)))


(leaf reference-packages
  :config
  (leaf use-package :ensure t)
  (leaf el-get :ensure t
    :init (unless (executable-find "git")
            (warn "'git' couldn't found. el-get can't download any packages"))
    :custom ((el-get-git-shallow-clone  . t)
             (el-get-emacswiki-base-url . "http://www.emacswiki.org/emacs/download/"))))


(leaf cus-start
  :doc "define customization properties of builtins"
  :custom `((gc-cons-threshold            . ,(* 512 1024 1024))
	    (garbage-collection-messages  . t)
	    (fill-column                  . 80)
	    (tab-width                    . 8)
	    ;; (shell-file-name . "/bin/bash")
	    (user-full-name               . "Naoya Yamashita")
	    (debug-on-error               . t)
	    (create-lockfiles             . nil)
	    (use-dialog-box               . nil)
            (use-file-dialog              . nil)
	    (frame-resize-pixelwise       . t)
            (enable-recursive-minibuffers . t)
            (history-length               . 1000)
            (history-delete-duplicates    . t)

            (menu-bar-mode                . t)
            (tool-bar-mode                . nil)
            (indent-tabs-mode             . nil))
  :config
  (put 'upcase-region   'disabled nil)
  (put 'downcase-region 'disabled nil))

(leaf mac
  :doc "Implementation of GUI terminal on macOS"
  :doc "Each SYMBOL can be `control', `meta', `alt', `hyper', or `super'"
  :doc "`left' meens same value setting its left key"
  :when (eq 'mac window-system)
  :custom ((mac-control-modifier       . 'control)
           (mac-option-modifier        . 'super)
           (mac-command-modifier       . 'meta)

           (mac-right-control-modifier . 'control)
           (mac-right-option-modifier  . 'hyper)
           (mac-right-command-modifier . 'meta)

           ;; use fn key as normal way.
           ;; (mac-function-modifier      . 'super)
           ))

(leaf ns
  :doc "NeXT/Open/GNUstep / macOS communication module"
  :when (eq 'ns window-system)
  :custom ((ns-control-modifier       . 'control)
           (ns-option-modifier        . 'super)
           (ns-command-modifier       . 'meta)

           (ns-right-control-modifier . 'control)
           (ns-right-option-modifier  . 'hyper)
           (ns-right-command-modifier . 'meta)

           ;; use fn key as normal way.
           ;; (ns-function-modifier      . 'super)
           (default-frame-alist . '((ns-appearance           . dark)
                                    (ns-transparent-titlebar . t)))))

(provide 'init)
;;; init.el ends here
