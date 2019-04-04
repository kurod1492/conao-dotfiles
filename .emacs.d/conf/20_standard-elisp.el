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
  :custom ((auto-revert-interval . 1))  ; check file every 1sec
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
  (leaf dired-filter :ensure t)
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
