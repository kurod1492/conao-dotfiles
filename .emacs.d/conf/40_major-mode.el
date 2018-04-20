;;; 40_major-mode.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@naoya-imac.local>
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

;;; Code:

(use-package php-mode :ensure t :defer t :disabled t
  :config
  (use-package web-mode :ensure t :defer t))

(use-package matlab-mode :ensure t :defer t)

(use-package plantuml-mode :ensure t :defer t
  :if (executable-find "plantuml")
  :config
  (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
  (setq plantuml-jar-path "/Users/conao/local/homebrew//opt/plantuml/libexec/plantuml.jar"))

(use-package org :ensure t :defer t
  :mode (("\\.txt$" . org-mode))
  :bind (("C-c o l" . org-store-link)
         ("C-c o a" . org-agenda)
         ("C-c o c" . org-capture)
         ("C-c o b" . org-iswitchb))

  :config
  (setq org-directory                         "~/Documents/org/"
        org-default-notes-file                "~/Documents/org/notes.org"
        org-agenda-files                      "~/Documents/org/notes.org"
        org-return-follows-link               t
        org-startup-indented                  t
        org-indent-mode-turns-on-hiding-stars t
        org-indent-indentation-per-level      2
        org-src-window-setup 'other-window
        org-highlight-latex-and-related '(latex script entities))
  (custom-set-faces '(org-latex-and-related ((t (:foreground "DeepSkyBlue2")))))

  (prog1 "setting src block faces"
    (defun modify-color (raw-color raw-shift)
      "COLOR<string> is RGB color like \"#999999\" or string like \"white\"
SHIFT<integer> or <list<integer>> is color shift num (r g b)"
      (let ((color '(0 0 0)) (shift '(0 0 0)))
        (setq raw-color (apply #'color-rgb-to-hex (color-name-to-rgb raw-color)))
        (if (listp raw-shift)
            (progn
              (setq shift (make-list 3 (first raw-shift)))
              (dotimes (i (if (<= (length raw-shift) 3) (length raw-shift) 3))
                (setf (nth i shift) (nth i raw-shift))))
          (setq shift (make-list 3 raw-shift)))
        (dotimes (i 3)
          (setf (nth i color) (string-to-number (substring raw-color
                                                           (+ 1 (* i 2))
                                                           (+ 1 (* (+ i 1) 2)))
                                                16))
          (setf (nth i color) (+ (nth i color) (nth i shift)))
          (setf (nth i color) (let ((x (nth i color)))
                                (cond ((< x 0) 0)
                                      ((> x 255) 255)
                                      (t x))))
          (setf (nth i color) (format "%02X" (nth i color))))
        (concat "#" (mapconcat #'identity color ""))))
    
    ;; foregroundをdefault（普通の色）と同じにする。
    ;; backgroundをdefault（普通の色）より少し暗くする。
    (set-face-foreground 'org-block (face-foreground 'default))
    (set-face-background 'org-block (modify-color (face-background 'default) -5))
    
    ;; 言語ごとに切り替えたい場合はこの変数で設定する。
    ;; org-src-block-facesが優先されるので、org-blockで設定した上で特に区別したい言語をこちらで設定するといい。
    ;; (setq org-src-block-faces `(("emacs-lisp" ((:background ,(modify-color (face-background 'default) -5))
    ;;                                            (:foreground ,(face-foreground 'default))))
    ;;                             ("python" (:background "#002030")))))
    )

  ;; org default package
  ;; (require 'org-macro)
  ;; (require 'org-element)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org-capture

  (setq org-capture-templates
        '(("t" "Todo" entry (file+headline "~/Dropbox/org/todo.org" "Tasks")
           "* TODO %?n %in %a")
          ("j" "Journal" entry (file+datetree "~/Dropbox/org/journal.org")
           "* %?(not ) %Un %in %a")
          ("n" "Note" entry (file+headline "~/Dropbox/org/notes.org" "Notes")
           "* %?n %Un %i")))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; table
  
  (use-package orgtbl-aggregate :ensure t)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; calendar
  (use-package japanese-holidays :ensure t :defer t
    :config
    (setq calendar-holidays               (append japanese-holidays
                                                  holiday-local-holidays holiday-other-holidays)
          calendar-mark-holidays-flag     t         ; 祝日をカレンダーに表示
          japanese-holiday-weekend        '(0 6)    ; 土日を祝日として表示
          japanese-holiday-weekend-marker '(holiday nil nil nil nil nil japanese-holiday-saturday)
                                        ; 土曜日を水色で表示
          org-agenda-include-diary        t)        ; org-agendaで祝日を表示する

    (add-hook 'calendar-today-visible-hook   'japanese-holiday-mark-weekend)
    (add-hook 'calendar-today-invisible-hook 'japanese-holiday-mark-weekend)
    ;; “きょう”をマークするには以下の設定を追加します。
    (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org babel
  
  ;; ipython
  (use-package ob-ipython :ensure t
    :config
    ;; depend of jypyter, ipython
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))

  ;; plantuml
  ;; depend on graphviz, plantuml
  ;; $ brew install graphviz plantuml
  ;; add 'export GRAPHVIZ_DOT=/Users/conao/local/homebrew/bin/dot' in .bashrc
  (if (file-exists-p "/usr/local/opt/plantuml/libexec/plantuml.jar")
      (setq org-plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar")
    (setq org-plantuml-jar-path "/Users/conao/local/homebrew//opt/plantuml/libexec/plantuml.jar"))

  ;; general settings
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (plantuml . t)
                                 ))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org exporting
  
  (use-package ox-latex)
  (use-package ox-novel :disabled t
    :init (el-get-bundle conao/ox-novel :branch "del-export-block"))
  (use-package ox-reveal :ensure t
    :init
    (el-get-bundle hakimel/reveal.js)
    (setq my-reveal-src-dir "~/.emacs.d/el-get/reveal.js"))
  (use-package org-install)
  (use-package org-bibtex)
  
  (prog1 "disable auto-save-buffer when src block editing"
    (defun disable-auto-save-when-enter-edit-special (&rest args)
      (progn
        (setq auto-save-buffers-active-p nil)
        (message "auto-save-buffers off")))
    (defun enable-auto-save-when-exit-edit-special (&rest args)
      (progn
        (setq auto-save-buffers-active-p t)
        (message "auto-save-buffers on")))
    
    (advice-add 'org-edit-special :before #'disable-auto-save-when-enter-edit-special)
    (advice-add 'org-edit-src-exit :after #'enable-auto-save-when-exit-edit-special))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; misc tools

  (use-package org-present :ensure t :disabled t)

  (prog1 "org-sparse-tree-buffer using indirect buffer"
    (defun org-sparse-tree-indirect-buffer (arg)
      (interactive "P")
      (let ((ibuf (switch-to-buffer (org-get-indirect-buffer))))
        (condition-case _
            (org-sparse-tree arg)
          (quit (kill-buffer ibuf)))))
    (bind-key "C-c /" 'org-sparse-tree-indirect-buffer org-mode-map))

  (use-package cdlatex :ensure t :defer t
    :init (use-package auctex :ensure t :defer t)
    :hook (org-mode . turn-on-org-cdlatex))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - latex export
  
  (setq org-html-htmlize-output-type 'css
        org-src-fontify-natively t
        org-latex-default-class "org-jsarticle"
        org-export-with-sub-superscripts '{}
        ;; org-latex-default-figure-position "H"
        )
  (setq org-export-in-background nil)
  (add-to-list 'org-latex-classes
               '("org-jsarticle"
                 "\\documentclass[platex]{jsarticle}
[NO-PACKAGES]
[NO-DEFAULT-PACKAGES]
\\usepackage[dvipdfmx,bookmarks=true,bookmarksnumbered=true]{hyperref}
\\usepackage[top=2truecm, bottom=2truecm, left=1.5truecm, right=1.5truecm, includefoot]{geometry}
\\usepackage[dvipdfmx]{graphicx,xcolor}
\\usepackage{fancyhdr}
\\usepackage{here}
\\usepackage{lscape}
\\usepackage{amsmath,amssymb}
\\pagestyle{fancy}
\\usepackage{pxjahyper}
\\usepackage{physics}
\\usepackage{framed, color}
\\usepackage{multicol}
\\usepackage{ulem}
\\usepackage{mdframed}
\\newcommand{\\up}{\\uparrow}
\\newcommand{\\rot}{\\nabla\\times}
\\newcommand{\\ee}{\\mathrm{e}}
\\newcommand{\\ii}{\\mathrm{i}}
\\newcommand{\\jj}{\\mathrm{j}}
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
             (setq org-latex-with-hyperref nil))

(provide '40_major-mode)
;;; 40_major-mode.el ends here