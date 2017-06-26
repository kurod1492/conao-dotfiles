;;; 40_major-mode.el --- config                      -*- lexical-binding: t; -*-

;; Copyright (C) 2017 conao

;; Author: conao
;; Keywords: 

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

(use-package org :ensure t :defer t
  :config
  ;; org default package
  ;; (require 'org-macro)
  ;; (require 'org-element)
  (use-package ox-latex)
  (use-package org-install)
  (use-package org-bibtex)

  ;; org extension package
  (use-package ox-novel
    :init (el-get-bundle conao/ox-novel :branch "del-export-block"))

  (prog1 "disable auto-save-buffer when src block editing"
    ())
  
  (setq org-startup-indented t
        org-indent-mode-turns-on-hiding-stars t
        org-indent-indentation-per-level 2
        org-html-htmlize-output-type 'css
        org-src-fontify-natively t
        org-latex-default-class "org-jsarticle")
  (add-to-list 'org-latex-classes
               '("org-jsarticle" "\\documentclass{jsarticle}
\\usepackage[top=2truecm, bottom=2truecm, left=1.5truecm, right=1.5truecm, includefoot]{geometry}
[NO-PACKAGES]
[NO-DEFAULT-PACKAGES]
\\usepackage[dvipdfmx]{graphicx,xcolor}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\rhead{\\thepage{}}"
             ("\\section{%s}" . "\\section*{%s}")
             ("\\subsection{%s}" . "\\subsection*{%s}")
             ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
             ("\\paragraph{%s}" . "\\paragraph*{%s}")
             ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))

             ;; LaTeX 形式のファイル PDF に変換するためのコマンド
             (setq org-latex-pdf-process
                       '("platex %f"
                         "platex %f"
                         "bibtex %b"
                         "platex %f"
                         "platex %f"
                         "dvipdfmx %b.dvi"
                         "rm %b.bbl %b.dvi"
                         ;; "find . -type f -name '*.xbb' -print0 | xargs -0 rm"
                        ))

             ;; \hypersetup{...} を出力しない
             (setq org-latex-with-hyperref nil)
;; (require ‘ess)
;; (org-babel-do-load-languages
;;  'org-babel-load-languages
;;  '((R . t)
;;    )
;;  )
;; (require 'org-babel-init)
;; (require 'org-babel-R)
;; (org-babel-load-library-of-babel)
;; (add-to-list 'auto-mode-alist '("\\.[rR]$" . R-mode))
;; R-mode を起動する時に ess-site をロード
;; (autoload 'R-mode "ess-site" "Emacs Speaks Statistics mode" t)
;; R を起動する時に ess-site をロード
;; (autoload 'R "ess-site" "start R" t)

;; (add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
;; (add-hook 'org-mode-hook 'org-display-inline-images))
)

(provide '40_major-mode)
;;; 40_major-mode.el ends here

