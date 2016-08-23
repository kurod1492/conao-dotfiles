;;; 20_editor.el ---

;; Copyright (C) 2016 Naoya Yamashita

;; Author: Naoya Yamashita
;; Keywords:

;;; Code:

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

