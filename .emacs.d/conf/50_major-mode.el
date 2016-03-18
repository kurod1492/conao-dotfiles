;;; 50_major-mode.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:25>
;; Last-Updated: <2016/03/05 01:10:03>
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
(use-package web-mode
  :ensure t
  :defer  t
  :mode  (("\\.html?\\'" . web-mode)
          ("\\.jsp\\'"   . web-mode)
          ("\\.gsp\\'"   . web-mode)
          ("\\.php\\'"   . web-mode))
  :config (setq web-mode-markup-indent-offset 4
                web-mode-css-indent-offset    4
                web-mode-code-indent-offset   4
                indent-tabs-mode              nil))

(use-package yatex
  :ensure t
  :defer  t
  :mode (("\\.tex" . yatex-mode)))

(use-package org
  :ensure t
  :defer  t
  :config (progn (require 'org-install)
                 (setq org-html-htmlize-output-type 'css)
                 (setq org-src-fontify-natively t)
                 (setq org-latex-default-class "org-jsarticle")
                 (add-to-list 'org-latex-classes
                              '("org-jsarticle" "\\documentclass{jsarticle}
\\usepackage[top=2truecm, bottom=2truecm, left=1.5truecm, right=1.5truecm, includefoot]{geometry}
[NO-PACKAGES]
[NO-DEFAULT-PACKAGES]
\\usepackage[dvipdfmx]{graphicx}
\\usepackage{fancyhdr}
\\pagestyle{fancy}
\\rhead{\\thepage{}}"
  ("\\section{%s}" . "\\section*{%s}")
  ("\\subsection{%s}" . "\\subsection*{%s}")
  ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
  ("\\paragraph{%s}" . "\\paragraph*{%s}")
  ("\\subparagraph{%s}" . "\\subparagraph*{%s}")))
                 (require 'ox-latex)
;;                  (require 'org-macro)
;;                  (require 'org-element)
                 (require 'ox-bibtex)

                 ;; LaTeX 形式のファイル PDF に変換するためのコマンド
                 (setq org-latex-pdf-process
                       '("platex %f"
                         "platex %f"
                         "bibtex %b"
                         "platex %f"
                         "platex %f"
                         "dvipdfmx %b.dvi"))

                 ;; \hypersetup{...} を出力しない
                 (setq org-latex-with-hyperref nil)))