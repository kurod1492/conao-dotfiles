;;; 10_standard-elisp.el ---                         -*- lexical-binding: t; -*-

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

;; custom file (setting at top)
(leaf cus-edit
  :custom (;; (custom-file . (locate-user-emacs-file "custom.el"))
           (custom-file . "/dev/null"))
  ;; :config
  ;; (when (file-readable-p custom-file)
  ;;   (load custom-file))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Resorces/lisp
;;

(leaf align
  ;; ==== function ====
  ;; align (beg end &optional separate rules exclude-rules)
  :doc "align text to a specific column, by regexp")

(leaf auth-source
  :doc "authentication sources for Gnus and Emacs"
  :init
  (leaf password-cache
    :init "Read passwords, possibly using a password cache"))

(leaf auth-source-pass
  :doc "Integrate auth-source with password-store")

(leaf autoinsert
  :doc "automatic mode-dependent insertion of text into new files")

(leaf autorevert
  ;; ==== minor mode ====
  ;; auto-revert-mode
  ;; auto-revert-tail-mode
  :doc "revert buffers when files on disk change"
  :init
  (leaf filenotify
    "watch files for changes on disk")
  :custom ((auto-revert-interval . 1)                ; check file every 1sec
           (global-auto-revert-non-file-buffers . t) ; revert dired and buffer-list
           )
  :config
  (global-auto-revert-mode))

(leaf custom
  :config
  ;; (load-theme 'wombat)
  ;; (leaf monokai-theme :ensure t
  ;;       :config (load-theme 'monokai t))
  ;; (leaf flucui-themes :ensure t
  ;;       :config (flucui-themes-load-style 'dark))
  ;; (leaf zenburn-theme :ensure t
  ;;       :config (load-theme 'zenburn t))
  (leaf sublime-themes :ensure t
        :config (load-theme 'brin t)))

(leaf dired
  :config
  (leaf dired-x)
  (leaf wdired
    :bind (:map dired-mode-map
                ("r" . wdired-change-to-wdired-mode)))
  (leaf dired-filter :ensure t
        :hooks (dired-mode-hook . dired-filter-mode))
  :custom ((dired-recursive-copies  . 'always)
           (dired-recursive-deletes . 'always)))

(leaf menu-bar
  :doc "define a default menu bar"
  :config
  (menu-bar-mode 0)
  (tool-bar-mode 0))

(leaf find-dired
  ;; ==== function ====
  ;; find-name-dired (find file recursive)
  :doc "run a `find' command and dired the output")

(leaf files
  :doc "file input and output commands for Emacs")

(leaf novice
  :doc "handling of disabled commands (\"novice mode\") for Emacs"
  :config
  (mapc (lambda (func) (put func 'disabled nil))
        '(narrow-to-region narrow-to-page narrow-to-defun list-timers)))

(leaf *mouse-support-on-tty
  :when (not window-system)
  :init (progn
          (defun conao3-scroll-down () (scroll-down 1))
          (defun conao3-scroll-up   () (scroll-up   1)))
  :bind (([mouse-4] . conao3-scroll-down)
         ([mouse-4] . conao3-scroll-up))
  :config
  (leaf xt-mouse
    :doc "support the mouse when emacs run in an xterm"
    :config
    (xterm-mouse-mode t))

  (leaf mouse
    :doc "window system-independent mouse support")

  (leaf mwheel
    :doc "Wheel mouse support"))

(leaf save-place-mode
  :doc "automatically save place in files"
  :config
  (save-place-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Resources/lisp/term
;;

(leaf mac-win
  :doc "parse switches controlling interface with Mac window system"
  :config
  (leaf *mac-auto-ascii-mode-fix
    :doc "fix Japanese/US input method issue"
    :init
    (defvar conao3/mac-last-input-method nil)
    (defun conao3/mac-save-input-source (&rest args)
      (when (not (equal (mac-input-source)
                        (mac-input-source 'ascii-capable-keyboard)))
        (setq conao3/mac-last-input-method (mac-input-source))))

    (defun conao3/mac-restore-input-source ()
      (when (and conao3/mac-last-input-method
                 (not (minibufferp)))
        (mac-select-input-source conao3/mac-last-input-method)
        (setq conao3/mac-last-input-method nil)))
    :hook (post-command-hook . conao3/mac-restore-input-source)
    :config
    (mac-auto-ascii-mode 1)
    (advice-add 'mac-auto-ascii-select-input-source :before #'conao3/mac-save-input-source)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Resources/lisp/progmodes
;;

(leaf grep
  ;; ==== function ====
  ;; rgrep (grep recursive)
  ;; lgrep (grep)
  :doc "run `grep' and display the results")

(leaf flymake
  :doc "A universal on-the-fly syntax checker")

(leaf paren
  :custom ((show-paren-delay . 0.0))
  :config
  (show-paren-mode 1))

(provide '10_standard-elisp)
;;; 10_standard-elisp.el ends here
