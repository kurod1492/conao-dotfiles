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


(prog1
  "Change user-emacs-directory"
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

(prog1
  "Load leaf.el"
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
  :config
  (leaf alloc.c
    :doc "Storage allocation and gc for GNU Emacs Lisp interpreter"
    :custom `((gc-cons-threshold           . ,(512 1024 1024)) ; alloc integer
              (gc-cons-percentage          . 0.1)    ; alloc integer
              (garbage-collection-messages . t)))    ; alloc boolean

  (leaf buffer.c
    :doc "Buffer manipulation primitives for GNU Emacs"
    :custom ((cursor-type                    . 'box) ; display ,cursor-type-types
	     ;; (mode-line-format mode-line sexp)    ; mode-line sexp
	     ;; (major-mode internal function)       ; internal function
	     (case-fold-search               . t)    ; matching boolean
	     (fill-column                    . 80)   ; fill integer
	     (left-margin                    . 0)    ; fill integer
	     (tab-width                      . 8)    ; editing-basics integer
	     (ctl-arrow                      . t)    ; display boolean
	     (truncate-lines                 . t)    ; display boolean
	     (word-wrap                      . nil)  ; display boolean
	     (selective-display-ellipses     . t)    ; display boolean
	     (indicate-empty-lines           . nil)  ; fringe boolean
	     (indicate-buffer-boundaries     . nil)  ; fringe sexp
	     (scroll-up-aggressively         . 0.0)  ; windows float
	     (scroll-down-aggressively       . 0.0)  ; windows float
	     (line-spacing                   . 0.0)  ; display float
	     (cursor-in-non-selected-windows . t)    ; cursor boolean
	     (transient-mark-mode            . t)    ; editing-basics boolean nil
	     (bidi-paragraph-direction       . 'left-to-right)))

  (leaf callint.c
    :doc "Call a Lisp function interactively"
    :custom ((mark-even-if-inactive . t)))   ; editing-basics boolean

  (leaf callproc.c
    :doc "Synchronous subprocess invocation for GNU Emacs"
    :custom ((shell-file-name . "/bin/bash") ; execute file
	     ;; (exec-path . "")             ; execute repeat
	     (exec-suffixes . "")))          ; execute (repeat string)

  (leaf charset.c
    :doc "Basic character set support"
    ;; :custom ((charset-map-path . ""))     ; installation repeat
    )

  (leaf coding.c
    :doc "Coding system handler (conversion, detection, etc)"
    :custom ((inhibit-eol-conversion       . nil)     ; mule boolean
	     (enable-character-translation . t)       ; mule boolean
	     (eol-mnemonic-undecided       . ":")     ; mule string
             (eol-mnemonic-unix            . ":")     ; mule string
	     (eol-mnemonic-dos             . "(DOS)") ; mule string
	     (eol-mnemonic-mac             . "(Mac)") ; mule string
	     (file-coding-system-alist     . nil)))   ; mule sexp

  (leaf dired.c
    :doc "Lisp functions for making directory listings"
    ;; :custom ((completion-ignored-extensions . nil))  ; dired sexp
    )

  (leaf dispnew.c
    :doc "Updating of data structures for redisplay"
    :custom ((baud-rate            . 38400) ; display integer
	     (inverse-video        . nil)   ; display boolean
	     (visible-bell         . nil)   ; display boolean
	     (no-redraw-on-reenter . nil))) ; display boolean

  (leaf doc.c
    :doc "Record indices of function doc strings stored in a file"
    :custom ((text-quoting-style   . nil))) ; display choice

  ;; (leaf dosfns.c
  ;;   :doc
  ;;   :custom ((dos-display-scancodes)    ; display boolean
  ;;            (dos-hyper-key)            ; keyboard integer
  ;;            (dos-super-key)            ; keyboard integer
  ;;            (dos-keypad-mode)))        ; keyboard integer

  (leaf editfns.c
    :doc "Lisp functions pertaining to editing"
    :custom ((user-full-name . "Naoya Yamashita"))) ; mail string

  (leaf emacs.c
    :doc "Fully extensible Emacs, running on Unix, intended for GNU"
    :custom ((report-emacs-bug-address . "bug-gnu-emacs@gnu.org"))) ; emacsbug string

  (leaf eval.c
    :doc "Evaluator for GNU Emacs Lisp interpreter"
    :custom ((max-specpdl-size             . 1300)   ; limits integer
	     (max-lisp-eval-depth          . 1000)   ; limits integer
	     (max-mini-window-height       . 0.25)   ; limits number
	     (debug-on-error               . t)      ; debug const
	     ;; (debug-ignored-errors      . nil)    ; debug (repeat (choice symbol regexp))
	     (debug-on-quit                . nil)    ; debug boolean
	     (debug-on-signal              . nil)    ; debug boolean
             (debugger-stack-frame-as-list . nil)))  ; debugger boolean "26.1"

  (leaf fileio.c
    :doc "File IO for GNU Emacs"
    :custom ((delete-by-moving-to-trash    . nil)    ; auto-save boolean "23.1"
	     (auto-save-visited-file-name  . nil)))  ; auto-save boolean

  (leaf filelock.c
    :doc "Lock files for editing"
    :custom ((create-lockfiles         . nil)        ; files boolean "24.3"
	     ;; (temporary-file-directory)           ; files directory
             ))

  (leaf fns.c
    :doc "Random utility Lisp functions"
    :custom ((use-dialog-box      . nil)             ; menu boolean "21.1"
	     (use-file-dialog     . nil)             ; menu boolean "22.1"
	     (focus-follows-mouse . nil)))           ; frames symbol

  (leaf fontset.c
    :doc "Fontset handler"
    ;; :custom ((vertical-centering-font-regexp))    ; display regexp
    )
  )
  

(provide 'init)
;;; init.el ends here
