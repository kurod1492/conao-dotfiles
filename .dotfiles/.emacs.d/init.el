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

  (prog1 "conao3 utility"
    (defmacro p (form)
      "Output expand  given FORM."
      `(progn
         (pp (macroexpand-1 ',form))
         nil))

    (defmacro po (form)
      "Output expand given FORM."
      `(progn
         (pp ,form)
         nil))

    (defmacro pl (form &optional stream)
      "Output list"
      `(progn
         (with-temp-buffer
           (insert (prin1-to-string ,form))
           (goto-char (point-min))
           (forward-char)
           (ignore-errors
             (while t (forward-sexp) (insert "\n")))
           (delete-char -1)
           (princ (buffer-substring-no-properties (point-min) (point-max))
                  (or ,stream standard-output))
           (princ "\n"))
         nil)))
  (global-unset-key (kbd "m-o"))
  (global-unset-key (kbd "m-t")))


(leaf conao3-packages
  :doc "elisp packages are developed by conao3"
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


(leaf emacs-buildin
  :custom ((custom-file . "/dev/null"))
  :config
  (leaf cus-edit
    :custom (;; (custom-file . (locate-user-emacs-file "custom.el"))
             (custom-file . "/dev/null"))
    ;; :config
    ;; (when (file-readable-p custom-file)
    ;;   (load custom-file))
    )

  (leaf cus-start
    :doc "define customization properties of builtins"
    :custom `((gc-cons-threshold              . ,(* 512 1024 1024))
              (garbage-collection-messages    . t)
              (fill-column                    . 80)
              (tab-width                      . 8)
              ;; (shell-file-name . "/bin/bash")
              (user-full-name                 . "Naoya Yamashita")
              (debug-on-error                 . t)
              (create-lockfiles               . nil)
              (use-dialog-box                 . nil)
              (use-file-dialog                . nil)
              (frame-resize-pixelwise         . t)
              (enable-recursive-minibuffers   . t)
              (history-length                 . 1000)
              (history-delete-duplicates      . t)
              (inhibit-compacting-font-caches . t)

              (truncate-lines   . t)
              (menu-bar-mode    . t)
              (tool-bar-mode    . nil)
              (indent-tabs-mode . nil))
    :config
    (mapc (lambda (func) (put func 'disabled nil))
          (cdr '(:dummy
                 upcase-region downcase-region
                 narrow-to-region narrow-to-page narrow-to-defun
                 list-timers))))

  (leaf mac
    :doc "implementation of gui terminal on macos"
    :doc "each symbol can be `control', `meta', `alt', `hyper', or `super'"
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
    :doc "next/open/gnustep / macos communication module"
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

  (leaf autorevert
    :doc "revert buffers when files on disk change"
    :custom ((auto-revert-interval . 1)
             (global-auto-revert-mode . t)))

  (leaf custom
    :when window-system
    :custom-face ((asdf . '((t :default asdf))))
    ;; :config
    ;; (load-theme 'wombat)
    ;; (leaf monokai-theme :ensure t
    ;;       :config (load-theme 'monokai t))
    ;; (leaf flucui-themes :ensure t
    ;;       :config (flucui-themes-load-style 'dark))
    ;; (leaf zenburn-theme :ensure t
    ;;       :config (load-theme 'zenburn t))
    ;; (leaf sublime-themes :ensure t
    ;;   :config (load-theme 'brin t))
    )

  (leaf paren
    :custom ((show-paren-delay . 0.0)
             (show-paren-mode  . t)))

  (leaf save-place-mode
    :doc "automatically save place in files"
    :custom ((save-place-mode . t)))

  (leaf dired
    :custom ((dired-recursive-copies  . 'always)
             (dired-recursive-deletes . 'always))
    :config
    (leaf dired-x
      :require t)
    (leaf wdired
      :bind (:dired-mode-map
             ("r" . wdired-change-to-wdired-mode)))
    (leaf dired-filter
      :ensure t
      :hook ((dired-mode-hook . dired-filter-mode)))))


(leaf minor-mode
  :config
  (leaf smartparens
    :doc "Automatic insertion, wrapping and  navigation with user defined pairs"
    :url "https://github.com/Fuco1/smartparens/wiki/Working-with-expressions"
    :url "https://github.com/Fuco1/smartparens/wiki/Tips-and-tricks"
    :when window-system
    :ensure t
    :require smartparens-config
    :custom ((sp-highlight-pair-overlay                     . nil)
             (sp-navigate-interactive-always-progress-point . t)
             (smartparens-global-strict-mode                . t))
    :bind (:smartparens-mode-map
           ;;;;
           ;;;; navigation

           ;; basic (fbnp-ae)
           ("C-M-f" . sp-forward-sexp)
           ("C-M-b" . sp-backward-sexp)
           ("C-M-n" . sp-next-sexp)
           ("C-M-p" . sp-previous-sexp)
           ("C-M-a" . sp-beginning-of-sexp)
           ("C-M-e" . sp-end-of-sexp)

           ;; checkin/checkout
           ("C-M-i" . sp-down-sexp)
           ("C-M-o" . sp-backward-up-sexp)

           ;; misc
           ("C-M-k"   . sp-kill-sexp)
           ("C-M-w"   . sp-copy-sexp)
           ("C-M-t"   . sp-transpose-sexp)
           ("C-M-SPC" . sp-mark-sexp)

           ;;;;
           ;;;; depth-changing commands

           ;; basic
           ("M-s"           . sp-splice-sexp)
           ("M-r"           . sp-splice-sexp-killing-around)
           ("M-<up>"        . nil)
           ("M-<down>"      . nil)
           ("M-("           . sp-wrap-round)
           ("M-["           . sp-wrap-square)
           ("M-{"           . sp-wrap-qurly)
           ("M-<delete>"    . sp-unwrap-sexp)
           ("M-<backspace>" . sp-backward-unwrap-sexp)

           ;; barf/slurp
           ("C-)" . sp-forward-slurp-sexp)
           ("C-}" . sp-forward-barf-sexp)
           ("C-(" . sp-backward-slurp-sexp)
           ("C-{" . sp-backward-barf-sexp)

           ;; split/join
           ("M-k" . sp-split-sexp)
           ("M-j" . sp-join-sexp)

           ;;;;
           ;;;; misc

           ;; change constructure
           ("C-c s a" . sp-absorb-sexp)
           ("C-c s e" . sp-emit-sexp)
           ("C-c s p" . sp-convolute-sexp)
           ("C-c s t" . sp-transpose-hybrid-sexp)

           ;; change elements
           ("C-c s (" . sp-rewrap-sexp)
           ("C-c s r" . sp-change-inner)
           ("C-c s s" . sp-change-encosing)))

  (leaf multiple-cursors
    :ensure t
    :bind (("M-u" . hydra-multiple-cursors/body))
    :config
    (with-eval-after-load 'hydra
      (defhydra hydra-multiple-cursors (:color pink :hint nil)
        "
									╔════════╗
    Point^^^^^^             Misc^^            Insert                            ║ Cursor ║
  ──────────────────────────────────────────────────────────────────────╨────────╜
     _k_    _K_    _M-k_    [_l_] edit lines  [_i_] numbers
     ^↑^    ^↑^     ^↑^     [_m_] mark all    [_a_] letters
    mark^^ skip^^^ un-mk^   [_s_] sort
     ^↓^    ^↓^     ^↓^
     _j_    _J_    _M-j_
  ╭──────────────────────────────────────────────────────────────────────────────╯
			   [_q_]: quit, [Click]: point
"
        ("l" mc/edit-lines :exit t)
        ("m" mc/mark-all-like-this :exit t)
        ("j" mc/mark-next-like-this)
        ("J" mc/skip-to-next-like-this)
        ("M-j" mc/unmark-next-like-this)
        ("k" mc/mark-previous-like-this)
        ("K" mc/skip-to-previous-like-this)
        ("M-k" mc/unmark-previous-like-this)
        ("s" mc/mark-all-in-region-regexp :exit t)
        ("i" mc/insert-numbers :exit t)
        ("a" mc/insert-letters :exit t)
        ("<mouse-1>" mc/add-cursor-on-click)
        ;; Help with click recognition in this hydra
        ("<down-mouse-1>" ignore)
        ("<drag-mouse-1>" ignore)
        ("q" nil))))

  (leaf flymake
    :bind (:flymake-mode-map
           ("M-n" . flymake-goto-next-error)
           ("M-p" . flymake-goto-prev-error))
    :config
    (leaf flymake-diagnostic-at-point
      :ensure t
      :custom ((flymake-diagnostic-at-point-timer-delay . 0.1)
               (flymake-diagnostic-at-point-error-prefix . " ► ")
               (flymake-diagnostic-at-point-display-diagnostic-function . 'flymake-diagnostic-at-point-display-popup))
      ;; or flymake-diagnostic-at-point-display-minibuffer
      :hook ((flymake-mode-hook . flymake-diagnostic-at-point-mode))))

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
          :config (add-hook 'emacs-lisp-mode-hook #'package-lint-setup-flymake)))
      :config (flycheck-package-setup)))

  (leaf company
    :ensure t
    :custom ((company-minimum-prefix-length . 1)
             (global-company-mode . t))
    :config
    (leaf company-box
      :ensure t
      :init (leaf all-the-icons :ensure t :require t)
      :hook ((company-mode-hook . company-box-mode)))

    (leaf company-quickhelp
      :ensure t
      :custom ((company-quickhelp-delay . 0.8))
      :bind (:company-active-map
             ("M-h" . company-quickhelp-manual-begin))
      :hook ((global-company-mode-hook . company-quickhelp-mode))))

  (leaf yasnippet
    :ensure t
    :custom ((yas-indent-line . 'fixed)
             (yas-global-mode . t))
    :bind (:yas-minor-mode-map
           ("C-c y i" . yas-insert-snippet)
           ("C-c y n" . yas-new-snippet)
           ("C-c y v" . yas-visit-snippet-file)
           ("C-c y l" . yas-describe-tables)
           ("C-c y g" . yas-reload-all))
    :config
    (leaf yasnippet-snippets :ensure t)
    (leaf yatemplate :ensure t))

  (leaf treemacs :ensure t)

  (leaf projectile
    :ensure t
    :bind (("M-o p" . projectile-command-map))
    :custom ((projectile-mode . t)))

  (leaf origami
    :ensure t
    :custom ((global-origami-mode . t)))

  (leaf undo-tree
    :ensure t
    :custom ((global-undo-tree-mode . t)))

  (leaf real-auto-save
    :ensure t
    :custom ((real-auto-save-interval . 0.3))
    :hook ((find-file-hook . real-auto-save-mode)))

  (leaf powerline
    :disabled t
    :ensure t
    :config (powerline-default-theme))

  (leaf rainbow-mode
    :ensure t
    :custom ((rainbow-html-colors-major-mode-list
              . '(css-mode html-mode php-mode nxml-mode xml-mode))
             (rainbow-x-colors-major-mode-list
              . '(emacs-lisp-mode lisp-interaction-mode c-mode c++-mode java-mode))
             (rainbow-latex-colors-major-mode-list . '(latex-mode))
             (rainbow-ansi-colors-major-mode-list  . '(sh-mode c-mode c++-mode))
             (rainbow-r-colors-major-mode-list     . '(ess-mode)))
    :hook emacs-lisp-mode-hook)

  (leaf which-key
    :ensure t
    :custom ((which-key-idle-delay . 3)
             (which-key-replacement-alist
              . '(((nil . "Prefix Command") . (nil . "prefix"))
                  ((nil . "\\`\\?\\?\\'") . (nil . "lambda"))
                  (("<left>") . ("←"))
                  (("<right>") . ("→"))
                  (("<\\([[:alnum:]-]+\\)>") . ("\\1"))))
             (which-key-mode . t))))


(leaf major-mode
  :config
  nil)


(leaf misc-tools
  :config
  (leaf simple-httpd :ensure t)

  (leaf macrostep
    :ensure t
    :bind (("C-c e" . macrostep-expand))))

(provide 'init)
;;; init.el ends here
