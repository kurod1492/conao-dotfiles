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
  (unless (fboundp 'locate-user-emacs-file)
    (defun locate-user-emacs-file (dir)
      (expand-file-name dir user-emacs-directory)))
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf.el"))
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf-keywords.el"))
  (require 'leaf)
  (require 'leaf-keywords)
  (leaf-keywords-init)
  (leaf leaf
    :doc "Symplify your init.el configuration"
    :doc "Initialize leaf dependent packages"
    :config
    (leaf package
      :custom ((package-archives . '(("org"   . "https://orgmode.org/elpa/")
                                     ("melpa" . "https://melpa.org/packages/")
                                     ("gnu"   . "https://elpa.gnu.org/packages/"))))
      :config (package-initialize))
    (leaf hydra
      :ensure t
      :config
      (leaf *hydra-posframe
        :when (version<= "26.1" emacs-version)
        :when window-system
        :custom ((hydra-hint-display-type . 'posframe))))))


(leaf *initialize-emacs
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

  (global-unset-key (kbd "M-o"))
  (global-unset-key (kbd "M-t")))


(leaf *conao3-packages
  :doc "elisp packages are developed by conao3"
  :init (leaf *dependency
	  :config
          ;; solarized
	  (leaf dash :ensure t)

          ;; phantom-inline-comment
	  (leaf popwin :ensure t))
  :config
  (leaf *melpa-packages
    :config
    (leaf seml-mode
      :when (version<= "25.1" emacs-version)
      :ensure t
      :custom ((seml-live-refresh-interval     . 0.35)
               (seml-live-refresh-url-variable . ":type/:var1/:var2")
               (seml-live-refresh-url-quety    . '(targetpath targetfile)))))

  (leaf *site-lisp-packages
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
      :when (version<= "25.1" emacs-version)
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
      :when (version<= "25.1" emacs-version)
      :load-path `,(locate-user-emacs-file "site-lisp/orglyth.el")
      :require t orglyth-html orglyth-latex)

    (leaf phantom-inline-comment
      :load-path `,(locate-user-emacs-file "site-lisp/phantom-inline-comment")
      :bind (("C-c b b" . phantom-inline-comment)
             ("C-c b d" . phantom-inline-comment-delete))
      :custom ((phantom-inline-comment-auto-save-mode    . t)
               (phantom-inline-comment-auto-restore-mode . t)))

    (leaf annotate
      :load-path `,(locate-user-emacs-file "site-lisp/annotate.el")
      :custom ((annotate-mode . t))
      :commands annotate-mode
      :bind (("C-c a a" . annotate-annotate)
             ("C-c a ]" . annotate-next-annotation)
             ("C-c a [" . annotate-previous-anotation)))

    (leaf point-history
      :load-path `,(locate-user-emacs-file "site-lisp/point-history")
      :custom ((point-history-mode . t))
      :bind (("C-c b p" . point-history-show)
             (:point-history-show-mode-map
              ("q" . point-history-close))))

    (leaf solarized-theme
      :when (version<= "25.1" emacs-version)
      :load-path `,(locate-user-emacs-file "site-lisp/solarized-emacs")
      :require t
      :custom ((solarized-use-less-bold  . t)
               (solarized-scale-org-headlines . nil)
               (solarized-distinct-fringe-background . t))
      :config
      (create-solarized-theme-with-palette 'dark 'solarized-wombat-dark
        '("#2a2a29" "#f6f3e8"           ; base03 (02 01 00 0 1 2) base3
          "#e5c06d" "#ddaa6f"           ; yellow orange
          "#ffb4ac" "#e5786d"           ; red    magenta
          "#834c98" "#a4b5e6"           ; violet blue
          "#7ec98f" "#8ac6f2"           ; cyan   green
          )
        (lambda ()
          (custom-theme-set-faces
           theme-name
           `(default ((,class (:foreground ,(solarized-color-blend base03 base3 0.15 2) :background ,base03))))
           `(highlight ((,class (:background ,violet))))
           `(font-lock-builtin-face ((,class (:foreground ,magenta))))
           `(font-lock-constant-face ((,class (:foreground ,blue))))
           `(font-lock-comment-face ((,class (:foreground ,base00))))
           `(mode-line
             ((,class (:foreground ,base2 :background ,(solarized-color-blend base03 base3 0.85 2)))))
           `(mode-line-inactive
             ((,class (:foreground ,base00 :background ,(solarized-color-blend base03 "black" 0.85 2)))))
           `(mode-line-buffer-id ((,class (:foreground ,base3 :weight bold))))
           `(minibuffer-prompt ((,class (:foreground ,base1)))))))
      (enable-theme 'solarized-wombat-dark))))


(leaf *reference-packages
  :config
  (leaf use-package :ensure t)
  (leaf el-get
    :ensure t
    :init (unless (executable-find "git")
            (warn "'git' couldn't found. el-get can't download any packages"))
    :custom ((el-get-git-shallow-clone  . t)
             (el-get-emacswiki-base-url . "http://www.emacswiki.org/emacs/download/"))))


(leaf *emacs-buildin
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
    :url "http://handlename.hatenablog.jp/entry/2011/12/11/214923" ; align sumple
    :bind (("M-ESC ESC" . keyboard-quit))
    ;; :hook ((before-save-hook . delete-trailing-whitespace))
    :custom `((gc-cons-threshold               . ,(* 512 1024 1024))
              (garbage-collection-messages     . t)
              (fill-column                     . 70)
              (tab-width                       . 8)
              ;; (shell-file-name              . "/bin/bash")
              (user-full-name                  . "Naoya Yamashita")
              (user-mail-address               . "conao3@gmail.com")
              (user-login-name                 . "conao3")
              (debug-on-error                  . t)
              (eval-expression-print-length    . nil)
              (eval-expression-print-level     . nil)
              (create-lockfiles                . nil)
              (use-dialog-box                  . nil)
              (use-file-dialog                 . nil)
              (frame-resize-pixelwise          . t)
              (enable-recursive-minibuffers    . t)
              (history-length                  . 1000)
              (history-delete-duplicates       . t)
              (inhibit-compacting-font-caches  . t)
              (scroll-preserve-screen-position . :always)

              (truncate-lines   . t)
              (menu-bar-mode    . t)
              (tool-bar-mode    . nil)
              (indent-tabs-mode . nil))
    :config
    (keyboard-translate ?\C-h ?\C-?)
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
             ;; https://www.reddit.com/r/emacs/comments/9jm1az/emacs_rendering_is_broken_in_macos_mojave/e6sg9ei/
             (default-frame-alist . '((inhibit-double-buffering . t)
                                      (ns-appearance            . dark)
                                      (ns-transparent-titlebar  . t)))))

  (leaf autorevert
    :doc "revert buffers when files on disk change"
    :custom ((auto-revert-interval . 1)
             (global-auto-revert-mode . t)))

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
    (leaf dired-x :require t)
    (leaf wdired
      :bind (:dired-mode-map
             ("r" . wdired-change-to-wdired-mode)))
    (leaf dired-filter
      :ensure t
      :hook ((dired-mode-hook . dired-filter-mode))))

  (leaf auth-source
    :url "http://aki2o.hatenablog.jp/entry/2014/09/20/Emacs%E3%81%AE%E8%A8%AD%E5%AE%9A%E3%81%A7%E3%81%AF%E3%83%91%E3%82%B9%E3%83%AF%E3%83%BC%E3%83%89%E3%81%A8%E3%81%8B%E3%81%AF%E7%9B%B4%E6%9B%B8%E3%81%8D%E3%81%9B%E3%81%9Aauth-source%E4%BD%BF%E3%81%86%E3%81%A8"
    :custom ((auth-sources . '("~/.secret/authinfo.gpg")))
    :preface
    (defun c/auth-source-get-passwd (&rest spec)
      (let ((founds (apply 'auth-source-search spec)))
        (when founds
          (funcall (plist-get (nth 0 founds) :secret))))))

  (leaf files
    :custom ((require-final-newline . t)))

  (leaf smie
    :url "https://qiita.com/kawabata@github/items/1a51ff1e22ad7ae824d5")

  (leaf recentf
    :custom ((recentf-exclude . '(".recentf"))
             (recentf-max-saved-items . 200))))


(leaf *minor-mode
  :config
  (leaf smartparens
    :doc "Automatic insertion, wrapping and  navigation with user defined pairs"
    :url "https://github.com/Fuco1/smartparens/wiki/Working-with-expressions"
    :url "https://github.com/Fuco1/smartparens/wiki/Tips-and-tricks"
    :when window-system
    :ensure t
    :leaf-defer nil
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
    :hydra (hydra-multiple-cursors
            (:color pink :hint nil)
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
            ("q" nil)))

  (leaf flymake
    :bind (:flymake-mode-map
           ("M-n" . flymake-goto-next-error)
           ("M-p" . flymake-goto-prev-error))
    :config
    (leaf flymake-diagnostic-at-point
      :when (version<= "26.1" emacs-version)
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

  (leaf flyspell
    :when (executable-find "aspell")
    :hook ((org-mode        . flyspell-mode)
           (yaml-mode       . flyspell-mode)
           (markdown-mode   . flyspell-mode)
           (git-commit-mode . flyspell-mode)
           (prog-mode       . flyspell-prog-mode))
    :custom ((flyspell-issue-message-flag . nil)
             (ispell-program-name         . "aspell")
             (ispell-extra-args           . '("--sug-mode=ultra" "--lang=en_US" "--run-together")))
    :config
    (leaf flyspell-correct-ivy
      :ensure t
      :bind (("C-M-i" . flyspell-correct-wrapper))
      :custom ((flyspell-correct-interface . #'flyspell-correct-ivy))))

  (leaf company
    :ensure t
    :leaf-defer nil
    :bind ((:company-active-map
            ("M-n" . nil)
            ("M-p" . nil)
            ("C-s" . company-filter-candidates)
            ("C-n" . company-select-next)
            ("C-p" . company-select-previous)
            ("<tab>" . company-complete-selection)))
    :custom ((company-idle-delay            . 0)
             (company-minimum-prefix-length . 1)
             (company-transformers          . '(company-sort-by-occurrence))
             (global-company-mode           . t))
    :config
    (leaf company-box
      :when (version<= "26.1" emacs-version)
      :ensure t
      :init (leaf all-the-icons :ensure t :require t)
      :hook ((company-mode-hook . company-box-mode)))

    (leaf company-quickhelp
      :when (display-graphic-p)
      :ensure t
      :custom ((company-quickhelp-delay . 0.8))
      :bind (:company-active-map
             ("M-h" . company-quickhelp-manual-begin))
      :hook ((global-company-mode-hook . company-quickhelp-mode)))

    (leaf company-math
      :ensure t
      :preface
      (defun c/latex-mode-setup ()
        (setq-local company-backends
                    (append '((company-math-symbols-latex
                               company-math-symbols-unicode
                               company-latex-commands))
                            company-backends)))
      :hook ((org-mode-hook . c/latex-mode-setup)
             (tex-mode-hook . c/latex-mode-setup))))

  (leaf yasnippet
    :ensure t
    :url "https://kiwanami.hatenadiary.org/entry/20110224/1298526678"
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

  (leaf posframe
    :ensure t
    :when (version<= "26.1" emacs-version)
    :when window-system
    :config
    (leaf ivy-posframe
      :doc "Using posframe to show Ivy"
      :after ivy
      :ensure t
      :custom ((ivy-posframe-mode . t)
               (ivy-posframe-height-alist . '((swiper . 30) (t . 40)))
               (ivy-posframe-display-functions-alist
                . '((swiper . nil) (t . ivy-posframe-display-at-frame-center)))
               (ivy-posframe-parameters . '((left-fringe . 10)))))

    (leaf company-posframe
      :doc "Use a posframe as company candidate menu"
      :ensure t
      :after company
      :custom ((company-posframe-mode . t)))

    (leaf flycheck-posframe
      :ensure t
      :after flycheck
      :custom ((flycheck-posframe-mode . t)))

    (leaf which-key-posframe
      :ensure t
      :after which-key
      :custom ((which-key-posframe-mode . t)))

    (leaf ddskk-posframe
      :doc "Show Henkan tooltip for ddskk via posframe"
      :after skk
      :el-get conao3/ddskk-posframe.el
      :custom ((ddskk-posframe-mode . t))))

  (leaf treemacs
    :when (version<= "25.2" emacs-version)
    :ensure t)

  (leaf projectile
    :when (version<= "25.1" emacs-version)
    :ensure t
    :leaf-defer nil
    :bind (("M-o p" . projectile-command-map))
    :custom ((projectile-mode . t)))

  (leaf skk
    :ensure ddskk
    :require t skk-study skk-hint
    :bind (("C-x j" . skk-auto-fill-mode))
    :custom `((default-input-method . "japanese-skk")
              (skk-user-directory   . ,(locate-user-emacs-file "skk/jisyo"))
              (skk-large-jisyo      . ,(locate-user-emacs-file "skk/jisyo/SKK-JISYO.L"))
              (skk-auto-insert-paren . t)
              (skk-use-auto-enclose-pair-of-region . t)
              (skk-undo-kakutei-return-previous-point . t)
              (skk-check-okurigana-on-touroku . 'ask)
              (skk-henkan-number-to-display-candidates . 15))
    :init
    (let ((skk-jisyo-dir (locate-user-emacs-file "skk/jisyo")))
      (unless (file-directory-p skk-jisyo-dir)
        (make-directory skk-jisyo-dir 'parent)
        (skk-get skk-jisyo-dir))))

  (leaf selected
    :ensure t
    :custom ((selected-global-mode . t))
    :leaf-autoload nil
    :preface
    (defun c/eval-region ()
      (interactive)
      (when mark-active
        (eval-region (region-beginning) (region-end) t)))
    :bind ((:selected-keymap
            ("g" . google-this-noconfirm)
            (";" . comment-dwim)
            ("=" . count-words-region)
            ("f" . describe-function)
            ("v" . describe-variable)
            ("e" . c/eval-region)
            ("w" . osx-dictionary-search-pointer)
            ("5" . query-replace-from-region)
            ("q" . keyboard-quit)
            ("t" . org-table-convert-region))))

  (leaf origami
    :ensure t
    :custom ((global-origami-mode . t)))

  (leaf undo-tree
    :ensure t
    :custom ((global-undo-tree-mode . t)))

  (leaf real-auto-save
    :ensure t
    :custom ((real-auto-save-interval . 0.3))
    :commands real-auto-save-activate-advice
    :hook ((find-file-hook . real-auto-save-mode))
    :config (real-auto-save-activate-advice))

  (leaf powerline
    :disabled t
    :ensure t
    :config (powerline-default-theme))

  (leaf neotree
    :ensure t
    :custom ((neo-theme        . 'ascii)
             (neo-persist-show . t)
             (neo-smart-open   . t)
             (neo-smart-open   . t))
    :bind (("s-o" . neotree-toggle)))

  (leaf page-break-lines
    :ensure t
    :custom ((global-page-break-lines-mode . t)))

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
             (which-key-mode . t)))

  (leaf dumb-jump
      :ensure t
      :custom ((dumb-jump-mode               . t)
               (dumb-jump-selector           . 'ivy)
               (dumb-jump-use-visible-window . nil))
      :bind (("s-." . dumb-jump-go)
             ("s-," . dumb-jump-back)))

  (leaf persp-mode
    :ensure t
    :leaf-defer nil
    :custom `((persp-keymap-prefix                 . ,(kbd "C-c p"))
              (persp-nil-name                      . "default")
              (persp-set-last-persp-for-new-frames . nil)

              (persp-mode . t))
    :hook ((emacs-startup-hook . toggle-frame-maximized))
    :config
    ;; NOTE: Redefine `persp-add-new' to raddress.
    ;; Issue: Unable to create/handle persp-mode
    ;; https://github.com/Bad-ptr/persp-mode.el/issues/96
    ;; https://github.com/Bad-ptr/persp-mode-projectile-bridge.el/issues/4
    ;; https://emacs-china.org/t/topic/6416/7
    ;; (defun* persp-add-new (name &optional (phash *persp-hash*))
    ;;   "Create a new perspective with the given `NAME'. Add it to `PHASH'.
    ;; Return the created perspective."
    ;;   (interactive "sA name for the new perspective: ")
    ;;   (if (and name (not (equal "" name)))
    ;;       (destructuring-bind (e . p)
    ;;           (persp-by-name-and-exists name phash)
    ;;         (if e p
    ;;           (setq p (if (equal persp-nil-name name)
    ;;       		nil (make-persp :name name)))
    ;;           (persp-add p phash)
    ;;           (run-hook-with-args 'persp-created-functions p phash)
    ;;           p))
    ;;     (message "[persp-mode] Error: Can't create a perspective with empty name.")
    ;;     nil))

    ;; Ignore temporary buffers
    ;; (add-hook 'persp-common-buffer-filter-functions
    ;;           (lambda (b) (or (string-prefix-p "*" (buffer-name b))
    ;;       	       (string-prefix-p "magit" (buffer-name b)))))

    (leaf *ivy-integration
      :url "https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el"
      :config
      (with-eval-after-load "ivy"
        (add-hook 'ivy-ignore-buffers
                  #'(lambda (b)
                      (when persp-mode
                        (let ((persp (get-current-persp)))
                          (if persp
                              (not (persp-contain-buffer-p b persp))
                            nil)))))

        (setq ivy-sort-functions-alist
              (append ivy-sort-functions-alist
                      '((persp-kill-buffer   . nil)
                        (persp-remove-buffer . nil)
                        (persp-add-buffer    . nil)
                        (persp-switch        . nil)
                        (persp-window-switch . nil)
                        (persp-frame-switch  . nil)))))))

  (leaf elscreen
    :disabled t
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
    ;;  :init (el-get-bundle c/elscreen-swap)
    :config
    (setq elscreen-tab-display-control nil)
    ;; (leaf session
    ;;   :init (el-get-bundle c/revive)
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
      :disabled t
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

  (leaf avy
    :ensure t
    :bind (("M-o a a" . avy-goto-word-1)
           ("M-o a c" . avy-goto-char-2))
    :config
    (leaf ace-link
      :doc "Quickly follow links"
      :doc "
(ace-link-setup-default) will bind `o' to
  `ace-link-info'         in Info-mode
  `ace-link-help'         in help-mode
  `ace-link-woman'        in woman-mode
  `ace-link-eww'          in eww-mode
  `ace-link-compilation'  in compilation-mode
  `ace-link-custom'       in custom-mode-map"
      :ensure t
      :bind (:org-mode-map
             :package org
             ("M-o a l" . ace-link-org))
      :config
      (leaf ace-link
        :disabled t
        :after gnus
        :bind ((:gnus-summary-mode-map
                ("M-o a l" . ace-link-gnus))
               (:gnus-article-mode-map
                ("M-o a l" . ace-link-gnus))))
      (ace-link-setup-default))

    (leaf ace-window
      :ensure t
      :bind (("M-o a w" . ace-window))))

  (leaf ivy
    :ensure t
    :leaf-defer nil
    :custom ((ivy-re-builders-alist . '((t      . ivy--regex-fuzzy)
                                        (swiper . ivy--regex-plus)))

             (ivy-mode     . t)
             (counsel-mode . t))
    :init
    (leaf *ivy-ui-requirements
      :config
      (leaf swiper :ensure t)
      (leaf counsel :ensure t))
    :bind* (("C-x C-r" . counsel-recentf)
            ("C-s" . swiper))
    :custom ((ivy-initial-inputs-alist   . nil)
             (counsel-yank-pop-separator . "\n----------\n")
             (counsel-grep-base-command  . "ag -S --noheading --nocolor --nofilename --numbers '%s' %s"))
    :config
    (leaf *other-ivy-packages
      :config
      (leaf ivy-hydra
        :doc "Additional key bindings for Ivy"
        :ensure t
        :bind (("C-c i i" . hydra-ivy/body)))

      (leaf ivy-xref
        :doc "Ivy interface for xref results"
        :when (version<= "25.1" emacs-version)
        :ensure t
        :custom ((xref-show-xrefs-function . #'ivy-xref-show-xrefs)))

      (leaf ivy-rich
        :doc "More friendly display transformer for ivy"
        :ensure t
        :custom ((ivy-rich-mode . t)))

      (leaf ivy-point-history
        :init (el-get-bundle SuzumiyaAoba/ivy-point-history)
        :bind (("C-c b p" . ivy-point-history))
        :require t))

    (leaf *ivy-integration
      :config
      ;; Integration with `projectile'
      (with-eval-after-load 'projectile
        (leaf counsel-projectile
          :ensure t
          :config (counsel-projectile-mode 1))
        (custom-set-variables
         '(projectile-completion-system 'ivy)))

      ;; Integration with `magit'
      (with-eval-after-load 'magit
        (custom-set-variables
         '(magit-completing-read-function 'ivy-completing-read)))))

  (leaf lsp-mode
    :url "https://github.com/emacs-lsp/lsp-mode#supported-languages"
    :url "https://github.com/MaskRay/ccls/wiki/lsp-mode#find-definitionsreferences"
    :doc "lsp is language server protocol"
    :when (version<= "25.1" emacs-version)
    :ensure t
    :custom (;; (lsp-inhibit-message . t)
             ;; (lsp-message-project-root-warning . t)

             ;; debug
             (lsp-print-io          . nil)
             (lsp-trace             . nil)
             (lsp-print-performance . nil)

             ;; general
             (lsp-auto-guess-root . t))
    :hook ((go-mode         . lsp)
           (c-mode          . lsp)
           (c++-mode        . lsp)
           (prog-major-mode . lsp-prog-major-mode-enable))
    :bind (:lsp-mode-map
           ("C-c r" . lsp-rename))
    :config
    (leaf *lsp-ui-requirements
      :config
      (leaf lsp-ui
        :ensure t
        :hook ((lsp-mode-hook . lsp-ui-mode)))

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
        ))

    (leaf *lsp-dap-mode
      :doc "dap is Debug Adapter Protocol"
      :config
      (leaf dap-mode
        :url "https://github.com/emacs-lsp/dap-mode"
        :ensure t
        :custom ((dap-mode    . t)
                 (dap-ui-mode . t))))

    (leaf *other-lsp-pacakges
      :config
      (leaf lsp-treemacs
        :doc "Show errors with treemacs interface"
        ;; ==== functions ====
        ;; lsp-treemacs-errors-list
        :ensure t)

      (leaf lsp-java-treemacs
        :disabled t
        :ensure t)

      (leaf lsp-origami
        :disabled t
        :ensure t
        :hook ((origami-mode-hook . lsp-origami-mode))))

    (leaf *lsp-clients
      :config
      (leaf lsp-ruby
        :doc "Ruby support for lsp-mode"
        :ensure t
        :hook ((ruby-mode-hook . lsp))
        ;; :hook ((ruby-mode-hook . lsp-ruby-enable))
        )

      (leaf lsp-java
        :doc "Java support for lsp-mode"
        :ensure t
        :hook ((java-mode-hook . lsp))
        :config
        (leaf dap-java))

      (leaf *lsp-latex
        :doc "Latex support for lsp-mode"
        :when (file-exists-p "/Users/conao/Develop/tex/texlab.jar")
        :hook ((tex-mode-hook   . lsp)
               (latex-mode-hook . lsp)
               (yatex-mode-hook . lsp))
        :config
        (defvar lsp-latex-java-executable "java")
        (defvar lsp-latex-java-argument-list '("-jar"))
        (defvar lsp-latex-texlab-jar-file "/Users/conao/Develop/tex/texlab.jar")
        (defvar lsp-latex-texlab-jar-argument-list '())
        (defun lsp-latex-new-connection ()
          ""
          (append
           (cons
            lsp-latex-java-executable
            lsp-latex-java-argument-list)
           (cons
            lsp-latex-texlab-jar-file
            lsp-latex-texlab-jar-argument-list)))

        (lsp-register-client
         (make-lsp-client :new-connection
                          (lsp-stdio-connection
                           #'lsp-latex-new-connection)
                          :major-modes '(tex-mode yatex-mode latex-mode)
                          :server-id 'texlab))))))


(leaf *major-mode
  :config
  (leaf plantuml-mode
    :when (version<= "25.1" emacs-version)
    :ensure t)
  (leaf polymode
    :when (version<= "25.1" emacs-version)
    :ensure t)
  (leaf yaml-mode     :ensure t)
  (leaf haskell-mode  :ensure t)
  (leaf fish-mode     :ensure t)
  (leaf rust-mode     :ensure t)

  (leaf tuareg
    :doc "OCaml mode for Emacs."
    :ensure t)

  (leaf web-mode
    :ensure t
    :mode ("\\.css\\'"
           "\\.js\\'" "\\.json\\'" "\\.p?html?\\'"
           "\\.php\\'" "\\.tsx\\'" "\\.vue\\'" "\\.xml\\'"))

  (leaf tide
    :ensure t
    :init (defun c/enable-tide ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode)))
    :hook
    (before-save-hook . tide-format-before-save)
    (web-mode-hook . c/enable-tide))

  (leaf *clojure-modes
    :when (version<= "25.1" emacs-version)
    :config
    (leaf clojure-mode  :ensure t)
    (leaf edn           :ensure t)
    (leaf cider         :ensure t))

  (leaf *docker-modes
    :config
    (leaf docker              :ensure t)
    (leaf dockerfile-mode     :ensure t)
    (leaf docker-compose-mode :ensure t)
    (leaf docker-tramp        :ensure t))

  (leaf cc-mode
    :bind (:c-mode-base-map
           ("C-c c" . compile))
    :preface
    (defun c/c-mode-common ()
      (c-set-style "bsd")
      (setq tab-width 4)
      (setq c-basic-offset 4))
    :hook ((c-mode-common . c/c-mode-common)))

  (leaf go-mode
    :ensure t
    :custom ((gofmt-command . "goimports"))
    :bind (:go-mode-map
           ("C-c C-n" . go-run)
           ("C-c ."   . go-test-current-test)
           ("C-c f"   . go-test-current-file)
           ("C-c a"   . go-test-current-project))
    :hook ((before-save-hook . gofmt-before-save))
    :config
    (leaf gotest :ensure t)
    (leaf go-tag :ensure t
      :custom (go-tag-args . '("-transform" "camelcase"))))

  (leaf bison-mode
    :ensure t
    :custom ((bison-rule-separator-column   . 4)
             (bison-rule-enumeration-column . 4)
             (bison-decl-type-column        . 4)
             (bison-decl-token-column       . 4)))

  (leaf org
    :init (leaf org-plus-contrib :ensure t)
    :bind (("M-o o c" . org-capture)
           ("M-o o a" . org-agenda)
           ("M-o o l" . org-store-link)
           (:org-mode-map
            ("C-c i" . org-clock-in)
            ("C-c o" . org-clock-out)
            ("C-c n" . org-narrow-to-subtree)
            ("C-c b" . org-narrow-to-block)
            ("C-c e" . org-set-effort)))
    :custom ((org-directory                         . "~/Documents/org/")
             (org-default-notes-file                . "~/Documents/org/notes.org")
             (org-agenda-files                      . "~/Documents/org/notes.org")
             (org-return-follows-link               . t)
             (org-startup-indented                  . t)
             (org-indent-mode-turns-on-hiding-stars . t)
             (org-indent-indentation-per-level      . 2)
             (org-src-window-setup                  . 'other-window)
             (org-use-sub-superscripts              . '{})
             (org-image-actual-width                . nil)
             (org-highlight-latex-and-related       . '(latex script entities)))
    :config
    (leaf *misc-tools
      :config
      (leaf org-bullets
        :ensure t
        :hook ((org-mode-hook . org-bullets-mode))))

    (leaf ob
      :custom ((org-confirm-babel-evaluate . nil))
      :config
      (org-babel-do-load-languages 'org-babel-load-languages
                                   '(;; (ipython . t)
                                     ;; (plantuml . t)
                                     (org . t)
                                     ;; (R . t)
                                     (C . t)
                                     (shell . t)
                                     (emacs-lisp . t)))
      :init
      (leaf ob-ipython
        :when (executable-find "jupyter")
        :ensure t
        :config
        ;; depend of jypyter, ipython
        (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

      (leaf ob-plantuml
        :when (executable-find "plantuml")
        :custom ((org-plantuml-jar-path . plantuml-jar-path)
                 (org-confirm-babel-evaluate . nil))))

    (leaf ox
      :custom ((org-export-backends . '(;; build-in
                                        ascii
                                        html latex beamer odt org
                                        ;; bibtex texinfo
                                        ;; confluence deck freemind groff icalendar
                                        ;; koma-letter man md rss s5 taskjuggler

                                        extra

                                        ;; optional ox packages
                                        latex-subfigure

                                        ;; optional backends
                                        qmd re-reveal
                                        )))
      :config
      (leaf *built-in-ox-packages
        :config
        (leaf ox-latex
          :disabled t
          :custom `((org-latex-default-class . "jsarticle")
                    (org-latex-classes
                     . '(("jsarticle"
                          "\\documentclass[uplatex, dvipdfmx]{jsarticle}"
                          ("\\section{%s}"       . "\\section*{%s}")
                          ("\\subsection{%s}"    . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))
                         ("beamer"
                          "\\documentclass[dvipdfmx,12pt]{beamer}"
                          ("\\section{%s}"       . "\\section*{%s}")
                          ("\\subsection{%s}"    . "\\subsection*{%s}")
                          ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                          ("\\paragraph{%s}"     . "\\paragraph*{%s}")
                          ("\\subparagraph{%s}"  . "\\subparagraph*{%s}"))))
                    (org-latex-hyperref-template
                     . "\\hypersetup{\n  pdfauthor={%a},\n  pdftitle={%t},\n  pdfkeywords={%k},
  pdfsubject={%d},\n  pdfcreator={%c},\n  pdflang={%L},\n  pdfborder={0 0 0},
  colorlinks=false,\n  linkcolor=blue\n}\n")
                    (org-latex-listings          . ,(and (executable-find "kpsewhich")
                                                         (not (string= (shell-command-to-string "kpsewhich listing.sty") ""))
                                                         t))
                    (org-latex-listings-langs
                     . '((emacs-lisp "Lisp") (lisp "Lisp") (clojure "Lisp")
                         (c "C") (cc "C++")
                         (fortran "fortran")
                         (perl "Perl") (cperl "Perl")
                         (python "Python") (ruby "Ruby")
                         (html "HTML") (xml "XML")
                         (tex "TeX") (latex "[LaTeX]TeX")
                         (shell-script "bash")
                         (gnuplot "Gnuplot")
                         (ocaml "Caml") (caml "Caml")
                         (sql "SQL") (sqlite "sql")
                         (makefile "make")
                         (R "r")
                         (shell "bash")))
                    (org-latex-listings-options . '(("basicstyle" "\\small")
                                                    ("keywordstyle" "\\color{black}\\bfseries\\underbar")))
                    (org-latex-compiler     . "uplatex")
                    (org-latex-bib-compiler . "bibtex")
                    (org-latex-pdf-process . '("latexmk -gg -pdflatex=\"%latex\" %f"))
                    (org-latex-logfiles-extensions
                     . '("aux" "bcf" "blg" "fdb_latexmk" "fls" "figlist" "idx" "log" "nav" "out"
                         "ptc" "run.xml" "snm" "toc" "vrb" "xdv" "dvi" "bbl"))
                    (org-latex-packages-alist
                     . '(
                         ;;;;;;;;;;;;;;;;;;;;
                         ;; org depends default packeages
                         
                         ("utf8" "inputenc")       ;; enable unicode input
                         ("T1" "fontenc")          ;; enable unicode output
                         ("" "graphicx")           ;; insert figures
                         ("" "grffile")            ;; enable strange filenames
                         ("" "longtable")          ;; long table with page break
                         ("" "wrapfig")            ;; text wrap figure
                         ("" "rotating")           ;; text rotate
                         ("normalem" "ulem")       ;; text decoration
                         ("" "textcomp")           ;; symbol font
                         ("" "capt-of")            ;; add caption at not float env
                         ("" "hyperref")           ;; hyperlink
                         ("" "amsmath, amssymb")   ;; math packages

                         ;;;;;;;;;;;;;;;;;;;;
                         ;; my optionnal packages
                         
                         ("" "pxjahyper")          ;; pdf bookmark label
                         ;; ("" "listings")           ;; code include
                         ;; ("" "fancyhdr")           ;; header, footer editing
                         ;; ("" "mdframed")           ;; framing
                         ;; ("" "here")               ;; figure put here
                         ;; ("" "lscape")             ;; landscape text, portrait page
                         ;; ("" "physics")            ;; math useful macros
                         ;; ("" "okumacro")           ;; useful macros by Dr.okumura
                         ;; ("" "framed")             ;; framing
                         ;; ("" "xcolor")             ;; pick color
                         ;; ("" "multicol")           ;; multi columns
                         ;; ("" "newtxtext")          ;; tx font
                         ;; ("" "newtxmath")          ;; tx math font
                         ("" "geometry")           ;; page layout
                         ;; ("" "mathtools")          ;; enhance the appearance for amsmath
                         ;; ("" "subcaption")         ;; multiple figures
                         "\\geometry{
top=2truecm, bottom=2truecm, left=1.5truecm, right=1.5truecm, includefoot}"
                         "\\pagestyle{fancy}"
                         "\\rhead{\\thepage{}}"
                         "\\mathtoolsset{showonlyrefs=true}"
                         ))))
        (leaf ox-extra
	  :when (version<= "25.1" emacs-version)
          :config
          (ox-extras-activate '(latex-header-blocks ignore-headlines))))

      (leaf *optional-ox-packages
        :config
        (leaf ox-qmd :ensure t)
        (leaf ox-latex-subfigure
          :init (el-get-bundle linktohack/ox-latex-subfigure))
        (leaf org-re-reveal
          :ensure t
          :custom ((org-re-reveal-root         . "https://cdnjs.cloudflare.com/ajax/libs/reveal.js/3.8.0")
                   (org-re-reveal-transition   . "slide")
                   (org-re-reveal-theme        . "conao3-dark")
                   (org-re-reveal-history      . t)
                   (org-re-reveal-center       . nil)
                   (org-re-reveal-slide-number . "c/t")
                   (org-re-reveal-margin       . "0.2")
                   (org-re-reveal-title-slide  . "
<h1 class=\"title\">%t</h1>
<h3 class=\"subtitle\">%s</h3>
<h3 class=\"author\">%a</h3>
<h4 class=\"date\">%d</h4>
<h4 class=\"miscinfo\">%m</h4>")))))))


(leaf *misc-tools
  :config
  (leaf *git-tools
    :config
    (leaf gitattributes-mode :ensure t)
    (leaf gitconfig-mode     :ensure t)
    (leaf gitignore-mode     :ensure t)
    (leaf gh                 :ensure t)
    (leaf github-pullrequest
      :when (version<= "25.1" emacs-version)
      :ensure t)

    (leaf git-messenger
      :ensure t
      :bind (("C-x v p" . git-messenger:popup-message)))

    (leaf magit
      :when (version<= "25.1" emacs-version)
      :ensure t
      :bind (("M-g s" . magit-status)))

    (leaf git-timemachine
      :doc "Walk through git revisions of a file"
      :doc "
p Visit previous historic version
n Visit next historic version
w Copy the abbreviated hash of the current historic version
W Copy the full hash of the current historic version
g Goto nth revision
t Goto revision by selected commit message
q Exit the time machine.
b Run magit-blame on the currently visited revision (if magit available).
c Show current commit using magit (if magit available).
"
      :when (version<= "25.1" emacs-version)
      :ensure t
      :bind (("M-g t" . git-timemachine-toggle)))

    (leaf diffview
      :doc "View diffs in side-by-side format"
      :ensure t))

  (leaf *http-tools
    :config
    (leaf simple-httpd :ensure t)
    (leaf restclient :ensure t)
    (leaf lingr :ensure t :commands lingr-login)

    (leaf google-translate
      :ensure t
      :custom ((google-translate-default-source-language . "en")
               (google-translate-default-target-language . "ja"))
      :bind (("C-c g" . google-translate-at-point)))

    (leaf request
      :doc "Compatible layer for URL request in Emacs"
      :doc "http://tkf.github.io/emacs-request/"
      :doc
      (request
       "https://api.github.com"
       :parser 'json-read
       :success (cl-function
                 (lambda (&key data &allow-other-keys)
                   (when data
                     (with-current-buffer (get-buffer-create "*request demo*")
                       (erase-buffer)
                       (insert (pp-to-string data))
                       (pop-to-buffer (current-buffer))))))
       :complete (lambda (&rest _) (message "Finished!"))
       :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                      (418 . (lambda (&rest _) (message "Got 418.")))))
      :ensure t))

  (leaf *grep-tools
    :config
    (leaf wgrep
      :ensure t
      :custom ((wgrep-enable-key . "e")
               (wgrep-auto-save-buffer . t)
               (wgrep-change-readonly-file . t)))

    (leaf ag
      :ensure t
      :custom ((ag-highligh-search . t)
               (ag-reuse-buffers . t)
               (ag-reuse-window . t))
      ;; :bind (("M-s a" . ag-project))
      :config
      (leaf wgrep-ag
        :ensure t
        :hook ((ag-mode-hook . wgrep-ag-setup))))

    (leaf migemo
      :disabled t
      :doc "Japanese incremental search through dynamic pattern expansion"
      :when (executable-find "cmigemo")
      :commands migemo-init
      :config
      (setq migemo-command (executable-find "cmigemo"))
      (autoload 'migemo-init "migemo" nil t)
      (migemo-init)))

  (leaf *misc-tools
    :config
    (leaf mew
      :url "http://sleepboy-zzz.blogspot.com/2012/11/mewgmail.html"
      :doc "brew install stunnel"
      :el-get `(mew
                :type git
                :build (("./configure" ,(format "--with-emacs=emacs-%s.%s"
                                                emacs-major-version emacs-minor-version)) "make")
                :url "https://github.com/kazu-yamamoto/Mew.git"
                :load-path "./"
                (add-to-list 'exec-path (expand-file-name "mew/bin" el-get-dir)))
      :config
      ;; (defconst my/mew-gmail-prefixes '(("default" "conao3@gmail.com")))
      ;; (setq mew-config-alist (my/mew-create-alist my/mew-gmail-prefixes))

      (setq mew-name "Naoya Yamashita") ;; (user-full-name)
      (setq mew-user "conao3") ;; (user-login-name)
      (setq mew-mail-domain "gmail.com")

      (setq mew-proto "%")

      (setq mew-imap-user "conao3@gmail.com")  ;; (user-login-name)
      (setq mew-imap-server "imap.gmail.com")    ;; if not localhost
      (setq mew-imap-auth  t)
      (setq mew-imap-ssl t)
      (setq mew-imap-ssl-port "993")

      (setq mew-smtp-user "conao3@gmail.com")
      (setq mew-smtp-server "smtp.gmail.com")  ;; if not localhost
      (setq mew-smtp-auth t)
      (setq mew-smtp-ssl t)
      (setq mew-smtp-ssl-port "465")

      (setq mew-fcc "+outbox") ; 送信メールを保存
      (setq mew-ssl-verify-level 0)
      (setq mew-use-cached-passwd t)
      (setq mew-use-master-passwd t)

      (setq mail-user-agent 'mew-user-agent)
      (define-mail-user-agent 'mew-user-agent 'mew-user-agent-compose 'mew-draft-send-message 'mew-draft-kill 'mew-send-hook))

    (leaf highlight-symbol
      :ensure t
      :bind (("C-x H" . hydra-highlight-symbol/body))
      :hydra (hydra-highlight-symbol
              (:post (progn
                       (highlight-symbol-remove-all)))
              "highlight-symbol"
              ("." highlight-symbol-at-point "highlight")
              ("n" highlight-symbol-next "next")
              ("p" highlight-symbol-prev "prev")
              ("N" highlight-symbol-next-in-defun "next in defun")
              ("P" highlight-symbol-prev-in-defun "prev in defun")))

    (leaf twittering-mode
      :doc "Major mode for Twitter"
      :url "https://github.com/hayamiz/twittering-mode"
      :ensure t)

    (leaf hide-mode-line
      :ensure t
      ;; :hook
      ;; ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
      )

    (leaf macrostep
      :ensure t
      :bind (("C-c e" . macrostep-expand)))

    (leaf dashboard
      :when (version<= "25.1" emacs-version)
      :ensure t
      :custom ((dashboard-items . '((recents . 15)
                                    (projects . 5)
                                    (bookmarks . 5)
                                    ;; (agenda . 5)
                                    )))
      :config
      (dashboard-setup-startup-hook))

    (leaf mwim
      :doc "Switch between the beginning/end of line or code"
      :ensure t
      :bind (("C-a" . mwim-beginning-of-code-or-line)
             ("C-e" . mwim-end-of-code-or-line)))

    (leaf google-this
      :ensure t
      :bind (("s-g" . google-this-noconfirm)))

    (leaf shell-pop
      :ensure t
      :bind (("C-o" . shell-pop)))

    (leaf latex-math-preview
      :when (version<= "25.1" emacs-version)
      :if (executable-find "platex")
      :ensure t
      :bind (("C-c l l" . latex-math-preview-expression)
             ("C-c l s" . latex-math-preview-insert-mathematical-symbol))
      :config
      (setq latex-math-preview-tex-to-png-for-preview '(platex dvips-to-eps gs-to-png)
            latex-math-preview-tex-to-png-for-save    '(platex dvipng)
            latex-math-preview-tex-to-eps-for-save    '(platex dvips-to-eps)
            latex-math-preview-tex-to-ps-for-save     '(platex dvips-to-ps)
            latex-math-preview-beamer-to-png          '(platex dvipdfmx gs-to-png)
            latex-math-preview-initial-page-of-symbol-list '((math . nil) (text . nil))
            latex-math-preview-latex-template-header
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
            )
      (with-eval-after-load 'latex-math-preview
        (add-to-list 'latex-math-preview-command-option-alist
                     '(gs-to-png "-q" "-dSAFER" "-dNOPAUSE" "-dBATCH" "-sDEVICE=pngalpha"
                                 "-dEPSCrop" "-r600" "-dTextAlphaBits=4"
                                 "-dGraphicsAlphaBits=4" "-dQUIET"))))))


(leaf *misc-functions
  :config
  (leaf *font-lock-util
    :url "https://buzztaiki.hatenablog.com/entry/20111209/1323444755"
    :preface
    (defun font-lock-user-keywords (mode &optional keywords)
      "Add user highlighting to KEYWORDS to MODE.
See `font-lock-add-keywords' and `font-lock-defaults'."
      (unless mode
        (error "mode should be non-nil "))
      (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
      (font-lock-add-keywords mode keywords)
      (put mode 'font-lock-user-keywords keywords))

    (defun c/kill-region-or-backward-kill-word ()
      (interactive)
      (if (region-active-p)
          (kill-region (point) (mark))
        (backward-kill-word 1)))
    :bind (("C-w" . c/kill-region-or-backward-kill-word))
    ;; :config
    ;; (font-lock-user-keywords
    ;;  'c-mode
    ;;  '(("!" . font-lock-warning-face)
    ;;    ("hoge" . font-lock-keyword-face)
    ;;    ("[0-9]+" . font-lock-constant-face)))
    ))

(provide 'init)
;;; init.el ends here
