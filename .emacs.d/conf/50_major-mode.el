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

(leaf org
  :init
  (leaf org-plus-contrib :ensure t :require nil)
  
  :config
  (setq org-directory                         "~/Documents/org/"
        org-default-notes-file                "~/Documents/org/notes.org"
        org-agenda-files                      "~/Documents/org/notes.org"
        org-return-follows-link               t
        org-startup-indented                  t
        org-indent-mode-turns-on-hiding-stars t
        org-indent-indentation-per-level      2
        org-src-window-setup                  'other-window
        org-use-sub-superscripts              '{}
        org-image-actual-width                nil
        org-highlight-latex-and-related '(latex script entities))
  
  (leaf ob
    :config
    (setq org-confirm-babel-evaluate nil)
    (org-babel-do-load-languages 'org-babel-load-languages
                                 '((ipython . t)
                                   (plantuml . t)
                                   (org . t)
                                   (R . t)
                                   (C . t)
                                   (emacs-lisp . t)))
    
    (leaf ob-ipython
      :if (executable-find "jupyter")
      :ensure t
      :config
      ;; depend of jypyter, ipython
      (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

    (leaf ob-plantuml
      :when (executable-find "plantuml")
      :ensure t
      :config
      (setq org-plantuml-jar-path plantuml-jar-path)))

  (leaf ox
    :config
    (leaf orglyth
      :config
      (leaf orglyth-html
        :config
        (setq orglyth-html-enable-option    t
              orglyth-html-use-ftp          nil
              orglyth-html-local-root-path  "~/Documents/sakura/orglyth/"
              orglyth-html-remote-root-path "~/Documents/sakura/remote/"
              orglyth-html-ftp-root-path    "/ftp:conao3@conao3.com:~/www/orglyth/")
        (orglyth-html-init)
        (orglyth-html-project-init)))))

(provide '40_major-mode)
;;; 40_major-mode.el ends here
