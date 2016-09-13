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
  ;;   :defer  t
  :config
  (require 'org-install)
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
;; (require 'org-macro)
;; (require 'org-element)
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

(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)
(add-hook 'org-mode-hook 'org-display-inline-images))
