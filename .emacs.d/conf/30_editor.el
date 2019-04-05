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

(leaf treemacs :ensure t)

(leaf simple-httpd :ensure t)

(leaf real-auto-save
  :ensure t
  :custom ((real-auto-save-interval . 0.3))
  :hook (find-file-hook . real-auto-save-mode))

(leaf which-key
  :ensure t
  :custom ((which-key-idle-delay . 2))
  :config (which-key-mode 1))

(leaf hungry-delete
  :ensure t
  :config (global-hungry-delete-mode 1))

(leaf shackle
  :ensure t
  :custom ((shackle-rules . '(("\*helm" :regexp t :align below :popup t :size 0.4))))
  :config (shackle-mode 1))

(leaf yasnippet
  ;; (expand-file-name "snippets" user-emacs-directory)
  :ensure t
  :bind (:map yas-minor-mode-map
              ("C-c y i" . yas-insert-snippet)
              ("C-c y n" . yas-new-snippet)
              ("C-c y v" . yas-visit-snippet-file)
              ("C-c y l" . yas-describe-tables)
              ("C-c y g" . yas-reload-all))
  :config
  (leaf yatemplate
    ;; (locate-user-emacs-file "templates")
    :ensure t)
  (yas-global-mode 1))

(leaf origami
  :ensure t
  :config (global-origami-mode 1))

(leaf undo-tree
  :ensure t
  :config
  (leaf undohist
    :ensure t
    :commands undohist-initialize
    :custom ((undohist-ignored-files . '("/tmp" "/elpa" "/el-get")))
    :config
    (autoload 'undohist-initialize "undohist" nil t)
    (undohist-initialize)
    (setq undohist-directory (locate-user-emacs-file "undohist")))
  (global-undo-tree-mode 1))

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

(leaf projectile
  :ensure t
  ;; :bind ("M-o p" . projectile-switch-project)
  :config
  (projectile-mode 1))

(leaf company
  :ensure t
  :config
  (leaf company-box
    :ensure t
    :hook (company-mode-hook . company-box-mode))

  (leaf company-quickhelp
    :ensure t
    :bind (:map company-active-map
		("M-h" . company-quickhelp-manual-begin))
    :hook (global-company-mode-hook . company-quickhelp-mode)
    :custom ((company-quickhelp-delay . 0.8)))

  (global-company-mode 1))

(leaf auto-complete
  :disabled t
  :ensure t
  :init (leaf fuzzy :ensure t)
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

(leaf flymake
  :config
  (use-package flymake-diagnostic-at-point
    :custom
    (flymake-diagnostic-at-point-timer-delay 0.1)
    (flymake-diagnostic-at-point-error-prefix " ")
    ;; or flymake-diagnostic-at-point-display-minibuffer
    (flymake-diagnostic-at-point-display-diagnostic-function 'flymake-diagnostic-at-point-display-popup)
    :hook
    (flymake-mode . flymake-diagnostic-at-point-mode)))

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
  :hook
  (org-mode . flyspell-mode)
  (yaml-mode . flyspell-mode)
  (markdown-mode . flyspell-mode)
  (git-commit-mode . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  ;; (flyspell-mode . (lambda ()
  ;;       	     (dolist (key '(;; "C-;"
  ;;                                   "C-," "C-."))
  ;;       	       (unbind-key key flyspell-mode-map))))
  :custom
  ((flyspell-issue-message-flag nil)
   (ispell-program-name "aspell")
   (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))
  :config
  (use-package flyspell-correct-ivy
    :bind ("C-M-i" . flyspell-correct-wrapper)
    :custom
    ((flyspell-correct-interface #'flyspell-correct-ivy))))

(leaf lsp-mode
  :url "https://github.com/emacs-lsp/lsp-mode#supported-languages"
  :url "https://github.com/MaskRay/ccls/wiki/lsp-mode#find-definitionsreferences"
  :doc "lsp is language server protocol"
  :ensure t
  :custom ((lsp-inhibit-message t)
           (lsp-message-project-root-warning t)
           (create-lockfiles nil))
  :hook (prog-major-mode . lsp-prog-major-mode-enable)
  :config
  (leaf *lsp-ui-requirements
    :config
    (leaf lsp-ui
      :ensure t
      :hook (lsp-mode-hook . lsp-ui-mode))

    (leaf company-lsp
      :ensure t
      :require t
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
      :require t
      :config (dap-mode 1) (dap-ui-mode 1)))

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
      :hooks (origami-mode-hook . lsp-origami-mode)))

  (leaf *lsp-clients
    :config
    (leaf lsp-ruby
      :doc "Ruby support for lsp-mode"
      :ensure t
      :hook
      (ruby-mode-hook . lsp)
      ;; :hook ((ruby-mode-hook . lsp-ruby-enable))
      )

    (leaf lsp-java
      :doc "Java support for lsp-mode"
      :ensure t
      :hook (java-mode-hook . lsp)
      :config
      (leaf dap-java))

    (leaf *lsp-latex
      :doc "Latex support for lsp-mode"
      :when (file-exists-p "/Users/conao/Develop/tex/texlab.jar")
      :hooks
      (tex-mode-hook   . lsp)
      (latex-mode-hook . lsp)
      (yatex-mode-hook . lsp)
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
                        :server-id 'texlab)))))

(leaf ivy
  :ensute t
  :init
  (leaf *ivy-ui-requirements
    :config
    (leaf swiper :ensure t)
    (leaf counsel :ensure t))
  :bind (("C-s" . swiper))
  :custom ((ivy-initial-inputs-alist   . nil)
           (counsel-yank-pop-separator . "\n----------\n")
           (counsel-grep-base-command  . "ag -S --noheading --nocolor --nofilename --numbers '%s' %s"))
  :config
  (leaf *other-ivy-packages
    :config
    (leaf ivy-xref
      :doc "Ivy interface for xref results"
      :ensure t
      :config (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

    (leaf flx
      :doc "fuzzy matching with good sorting"
      :ensure t)

    (leaf amx
      :doc "Alternative M-x with extra features"
      :ensure t)

    (leaf ivy-rich
      :doc "More friendly display transformer for ivy"
      :ensure t
      :config (ivy-rich-mode 1)))

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
       '(magit-completing-read-function 'ivy-completing-read))))

  (leaf *ivy-settings
    :config
    (ivy-mode 1)
    (counsel-mode 1)
    (setq ivy-re-builders-alist '((t . ivy--regex-fuzzy)))))

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

(leaf persp-mode
  ;; :hook ((emacs-startup . toggle-frame-maximized))
  :custom ((persp-keymap-prefix    . (kbd "C-c p"))
           (persp-nil-name         . "default")
           (persp-auto-resume-time . 1))
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
        	      (persp-frame-switch  . nil))))))

  (leaf *persp-settings
    :config
    (persp-mode 1)))

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

(provide '20_editor)
;;; 20_editor.el ends here
