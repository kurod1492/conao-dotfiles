;;; 40_major-mode.el ---                             -*- lexical-binding: t; -*-

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

(leaf plantuml-mode :ensure t)
(leaf polymode      :ensure t)
(leaf web-mode      :ensure t)
(leaf yaml-mode     :ensure t)
(leaf haskell-mode  :ensure t)

(leaf cider :ensure t :require t
      :init (leaf clojure-mode :ensure t))

(leaf *docker-modes
  :config
  (leaf docker :ensure t)
  (leaf dockerfile-mode :ensure t)
  (leaf docker-compose-mode :ensure t)
  (leaf docker-tramp :ensure t))

(leaf cc-mode
  :bind (:map c-mode-base-map
	      ("C-c c" . compile))
  :init
  (defun conao3/c-mode-common ()
    (c-set-style "bsd")
    (setq tab-width 4)
    (setq c-base-offset 4))
  :hook (c-mode-common . conao3/c-mode-common))

(leaf go-mode
  :ensure t
  :custom ((gofmt-command . "goimports"))
  :bind (:map go-mode-map
	      ("C-c C-n" . go-run)
	      ("C-c ."   . go-test-current-test)
	      ("C-c f"   . go-test-current-file)
	      ("C-c a"   . go-test-current-project))
  :hook (before-save-hook . gofmt-before-save)
  :config
  (leaf gotest :ensure t)
  (leaf go-tag :ensure t
    :config (setq go-tag-args (list "-transform" "camelcase"))))

(leaf bison-mode
  :ensure t
  :setq ((bison-rule-separator-column   . 4)
         (bison-rule-enumeration-column . 4)
         (bison-decl-type-column        . 4)
         (bison-decl-token-column       . 4)))

(leaf rust-mode :ensure t)

(leaf org
  :init
  (leaf org-plus-contrib :ensure t :require nil)
  
  :custom
  ((org-directory                         . "~/Documents/org/")
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
      :hooks (org-mode-hook . org-bullets-mode)))
  
  (leaf ob
    :setq ((org-confirm-babel-evaluate . nil))
    :config
    (leaf ob-ipython
      :when (executable-find "jupyter")
      :ensure t
      :config
      ;; depend of jypyter, ipython
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

    (leaf ob-plantuml
      :when (executable-find "plantuml")
      :ensure t
      :setq ((org-plantuml-jar-path . plantuml-jar-path))
      :setq ((org-confirm-babel-evaluate . nil)))

    (org-babel-do-load-languages 'org-babel-load-languages
                                 '(;; (ipython . t)
                                   ;; (plantuml . t)
                                   (org . t)
                                   ;; (R . t)
                                   (C . t)
                                   (shell . t)
                                   (emacs-lisp . t)))

    ;; override `org-eldoc-get-mode-local-documentation-function'
    (defun org-eldoc-get-mode-local-documentation-function (lang)
      "Check if LANG-mode sets eldoc-documentation-function and return its value."
      (let ((cached-func (gethash lang org-eldoc-local-functions-cache 'empty))
            ;; (mode-func (intern-soft (format "%s-mode" lang)))
            (mode-func (org-src--get-lang-mode lang))
            doc-func)
        (if (eq 'empty cached-func)
            (when (fboundp mode-func)
              (with-temp-buffer
                (funcall mode-func)
                (setq doc-func (and eldoc-documentation-function
                                    (symbol-value 'eldoc-documentation-function)))
                (puthash lang doc-func org-eldoc-local-functions-cache))
              doc-func)
          cached-func))))

  (leaf ox
    :config
    (leaf ox-qmd :ensure t)
    (leaf ox-latex-subfigure
      :init (el-get-bundle linktohack/ox-latex-subfigure))))

(provide '40_major-mode)
;;; 40_major-mode.el ends here
