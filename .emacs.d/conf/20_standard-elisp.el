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
  :custom ((dired-recursive-copies  . 'always)
           (dired-recursive-deletes . 'always)))

(leaf menu-bar
  :doc "define a default menu bar"
  :config
  (menu-bar-mode 0)
  (tool-bar-mode 0))

(leaf find-dired
  :doc "run a `find' command and dired the output"
  ;; ==== function ====
  ;; find-name-dired (find file recursive)
  )

(leaf files
  :doc "file input and output commands for Emacs"
  :config
  (global-auto-revert-mode))

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
  :doc "run `grep' and display the results"
  ;; ==== function ====
  ;; rgrep (grep recursive)
  ;; lgrep (grep)
  )

(leaf flymake
  :doc "A universal on-the-fly syntax checker")

(leaf paren
  :custom ((show-paren-delay . 0.0))
  :config
  (show-paren-mode 1))

(provide '10_standard-elisp)
;;; 10_standard-elisp.el ends here
