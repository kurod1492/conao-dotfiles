;;; 30_utility.el ---                                -*- lexical-binding: t; -*-

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

(leaf leaf-browser
  :config
  (leaf htmlize :ensure t)
  (leaf simple-httpd :ensure t
        :custom ((httpd-show-backtrace-when-error . t)))
  (leaf elquery :ensure t))

(leaf shell-pop
  :ensure t
  :bind (("C-o" . shell-pop)))

(leaf latex-math-preview
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
                             "-dGraphicsAlphaBits=4" "-dQUIET"))))
(provide '30_utility)
;;; 30_utility.el ends here
