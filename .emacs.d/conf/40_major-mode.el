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

(use-package generic-x :defer t
  :mode (("\\.htaccess\\'" . apache-conf-generic-mode)))

(use-package cc-mode :defer t
  :config
  (use-package c-eldoc :ensure t
    :hook ((c-mode . c-eldoc-settings))
    :config
    (defun c-eldoc-settings ()
      (set (make-local-variable 'eldoc-idle-delay) 0.20)
      (c-turn-on-eldoc-mode))))

(use-package csharp-mode :ensure t :defer t
  :mode "\\.cs\\'")

(use-package cperl-mode :defer t
  :config
  ;; cperl-mode is preferred to perl-mode
  (defalias 'perl-mode 'cperl-mode))

(use-package web-mode :ensure t :defer t
  :mode "\\.php\\'")

(use-package matlab-mode :ensure t :defer t)

(use-package ess :ensure t)

(use-package plantuml-mode :ensure t :defer t
  :commands (load-plantuml-mode)
  :init (defun load-plantuml-mode () t)
  ;; depend on graphviz, plantuml
  ;; $ brew install graphviz plantuml
  ;; add 'export GRAPHVIZ_DOT=/Users/conao/local/homebrew/bin/dot' in .bashrc
  
  :if (executable-find "plantuml")
  :config
  (cond ((file-exists-p "/usr/local/opt/plantuml/libexec/plantuml.jar")
           (setq plantuml-jar-path "/usr/local/opt/plantuml/libexec/plantuml.jar"))
          ((file-exists-p "/Users/conao/local/homebrew/opt/plantuml/libexec/plantuml.jar")
           (setq plantuml-jar-path "/Users/conao/local/homebrew/opt/plantuml/libexec/plantuml.jar"))))

(use-package org :ensure t :defer t
  :after (plantuml-mode)
  :demand t
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
        org-src-window-setup                  'other-window
        org-use-sub-superscripts              nil
        org-highlight-latex-and-related '(latex script entities))
  (let ((user-emacs-directory (file-name-directory (directory-file-name user-emacs-directory))))
    (setq org-export-async-init-file (user-setting-directory "init_orgexport.el")))
  
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
    :if (executable-find "jupyter")
    :config
    ;; depend of jypyter, ipython
    (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append))
  (use-package ob-plantuml
    :if (executable-find "plantuml")
    :config
    (use-package plantuml-mode)
    (setq org-plantuml-jar-path plantuml-jar-path))

  ;; general settings
  (setq org-confirm-babel-evaluate nil)
  (org-babel-do-load-languages 'org-babel-load-languages
                               '((ipython . t)
                                 (plantuml . t)
                                 ))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; org exporting

  (use-package orglyth :disabled t
    :init (el-get-bundle conao/orglyth)
    :config
    (use-package orglyth-html
      :config
      (setq orglyth-html-enable-opiton    t
            orglyth-html-use-ftp          nil
            orglyth-html-local-root-path  "~/public_html/orglyth/"
            orglyth-html-remote-root-path "~/public_html/remote/"
            orglyth-html-ftp-root-path    "/ftp:conao3@conao3.com:~/www/orglyth/")
      (orglyth-html-reset-variables)))
  
  (use-package ox-odt)
  (use-package ox-md)
  (use-package ox-org)
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

  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - html export

  (use-package orglyth
    :init (add-to-list 'load-path (concat el-get-dir "/orglyth/"))
    :config
    (use-package orglyth-html
      :config
      (setq orglyth-html-local-root-path  "~/public_html/orglyth/"
            orglyth-html-remote-root-path "~/public_html/remote/"
            orglyth-html-ftp-root-path    "/ftp:conao3@conao3.com:~/www/orglyth/")))
  
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; - latex export
  (use-package ox-latex
    :config
    (use-package ox-latex-subfigure
      :init (el-get-bundle linktohack/ox-latex-subfigure))
    
    (setq org-html-htmlize-output-type 'css
          org-src-fontify-natively t
          org-latex-default-class "org-jsarticle"
          org-export-with-sub-superscripts '{}
          ;; org-latex-default-figure-position "H"
          )
    (setq org-export-in-background nil)
    (setq org-latex-default-packages-alist nil)

    (add-list-to-list 'org-latex-packages-alist
                      '(
                        ;;;;;;;;;;;;;;;;;;;;
                        ;; org depends default packeages
                        
                        ("utf8" "inputenc")       ;; enable unicode input
                        ("T1" "fontenc")          ;; enable unicode output
                        ("" "graphicx")           ;; insert figures
                        ("" "grffile")            ;; enable strange filenames
                        ("" "longtable")          ;; long table with page break
                        ("" "wrapfig")            ;; text wrap figure
                        ("" "rotating")           ;; text rotate
                        ("normalem" "ulem")       ;; text decoration
                        ("" "textcomp")           ;; symbol font
                        ("" "capt-of")            ;; add caption at not float env
                        ("" "hyperref")           ;; hyperlink
                        ("" "amsmath, amssymb")   ;; math packages

                        ;;;;;;;;;;;;;;;;;;;;
                        ;; my optionnal packages
                        
                        ("" "pxjahyper")          ;; pdf bookmark label
                        ("" "listings")           ;; code include
                        ("" "fancyhdr")           ;; header, footer editing
                        ("" "mdframed")           ;; framing
                        ("" "here")               ;; figure put here
                        ("" "lscape")             ;; landscape text, portrait page
                        ("" "physics")            ;; math useful macros
                        ("" "okumacro")           ;; useful macros by Dr.okumura
                        ("" "framed")             ;; framing
                        ("" "xcolor")             ;; pick color
                        ("" "multicol")           ;; multi columns
                        ("" "newtxtext")          ;; tx font
                        ("" "newtxmath")          ;; tx math font
                        ("" "geometry")           ;; page layout
                        ("" "mathtools")          ;; enhance the appearance for amsmath
                        ("" "subcaption")         ;; multiple figures
                        "\\geometry{
top=2truecm, bottom=2truecm, left=1.5truecm, right=1.5truecm, includefoot}"
                        "\\pagestyle{fancy}"
                        "\\rhead{\\thepage{}}"
                        "\\mathtoolsset{showonlyrefs=true}"
                        ))
    
    (when (executable-find "kpsewhich")
      ;; unicode code include
      (unless (string= (shell-command-to-string "kpsewhich jlisting.sty") "")
        (add-list-to-list 'org-latex-packages-alist
                          '(("" "jlisting")) t)
        (setq org-latex-listings         'listings
              org-latex-listings-options nil))
      (unless (string= (shell-command-to-string "kpsewhich listingsextra.sty") "")
        (add-list-to-list 'org-latex-packages-alist
                          '(("" "listingsextra")) t))
      (unless (string= (shell-command-to-string "kpsewhich listingssetup.sty") "")
        (add-list-to-list 'org-latex-packages-alist
                          '(("" "listingssetup")) t)))
    
    (add-list-to-list 'org-latex-classes
                      '(("org-jsarticle"
                         "\\documentclass[uplatex, dvipdfmx]{jsarticle}"
                         ("\\section{%s}" . "\\section*{%s}")
                         ("\\subsection{%s}" . "\\subsection*{%s}")
                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                         ("\\paragraph{%s}" . "\\paragraph*{%s}")
                         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                        
                        ("org-beamer"
                         "\\documentclass[dvipdfmx,12pt]{beamer}"
                         ("\\section{%s}" . "\\section*{%s}")
                         ("\\subsection{%s}" . "\\subsection*{%s}")
                         ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                         ("\\paragraph{%s}" . "\\paragraph*{%s}")
                         ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

             ;; LaTeX 形式のファイル PDF に変換するためのコマンド
             (setq org-latex-pdf-process
                       '("uplatex %f"
                         "uplatex %f"
                         "bibtex %b"
                         "uplatex %f"
                         "uplatex %f"
                         "dvipdfmx %b.dvi"
                         ;; "find . -type f -name '*.xbb' -print0 | xargs -0 rm"
                        ))

             (setq org-latex-hyperref-template
  "\\hypersetup{
  pdfauthor={%a},
  pdftitle={%t},
  pdfkeywords={%k},
  pdfsubject={%d},
  pdfcreator={%c},
  pdflang={%L},
  pdfborder={0 0 0},
  colorlinks=false,
  linkcolor=blue
}
")
             (add-list-to-list 'org-latex-listings-langs '((shell "bash")
                                                           ))
             (add-list-to-list 'org-latex-logfiles-extensions '("dvi" "bbl"))
             ))

(provide '40_major-mode)
;;; 40_major-mode.el ends here
