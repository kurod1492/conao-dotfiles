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

(leaf *git-tools
  :config
  (leaf gitattributes-mode :ensure t)
  (leaf gitconfig-mode     :ensure t)
  (leaf gitignore-mode     :ensure t)
  (leaf gh                 :ensure t)
  (leaf github-pullrequest :ensure t)

  (leaf git-messenger
    :ensure t
    :bind (("C-x v p" . git-messenger:popup-message)))

  (leaf magit
    :ensure t
    :bind (("M-g s" . magit-status)))

  (leaf git-timemachine
    :doc "Walk through git revisions of a file"
    :doc "
p Visit previous historic version
n Visit next historic version
w Copy the abbreviated hash of the current historic version
W Copy the full hash of the current historic version
g Goto nth revision
t Goto revision by selected commit message
q Exit the time machine.
b Run magit-blame on the currently visited revision (if magit available).
c Show current commit using magit (if magit available).
"
    :ensure t
    :bind (("M-g t" . git-timemachine-toggle)))

  (leaf diffview
    :doc "View diffs in side-by-side format"
    :ensure t))

(leaf *http-tools
  :config
  (leaf restclient :ensure t)
  (leaf lingr :ensure t :require t)

  (leaf google-translate
  :ensure t
  :custom ((google-translate-default-source-language . "en")
           (google-translate-default-target-language . "ja"))
  :bind (("C-c g" . google-translate-at-point))))

(leaf *grep-tools
  :config
  (leaf wgrep
    :ensure t
    :custom ((wgrep-enable-key . "e")
             (wgrep-auto-save-buffer . t)
             (wgrep-change-readonly-file . t)))

  (leaf ag
    :ensure t
    :custom ((ag-highligh-search . t)
             (ag-reuse-buffers . t)
             (ag-reuse-window . t))
    ;; :bind (("M-s a" . ag-project))
    :config
    (leaf wgrep-ag
      :ensure t
      :hook (ag-mode-hook . wgrep-ag-setup)))

  (leaf migemo
    :disabled t
    :doc "Japanese incremental search through dynamic pattern expansion"
    :when (executable-find "cmigemo")
    :commands migemo-init
    :config
    (setq migemo-command (executable-find "cmigemo"))
    (autoload 'migemo-init "migemo" nil t)
    (migemo-init)))

(leaf *misc-tools
  :config
  (leaf all-the-icons
    :ensure t)

  (leaf hide-mode-line
    :ensure t
    ;; :hook
    ;; ((neotree-mode imenu-list-minor-mode minimap-mode) . hide-mode-line-mode)
    )

  (leaf request
    :doc "Compatible layer for URL request in Emacs"
    :doc "http://tkf.github.io/emacs-request/"
    :doc
    (request
     "https://api.github.com"
     :parser 'json-read
     :success (cl-function
               (lambda (&key data &allow-other-keys)
                 (when data
                   (with-current-buffer (get-buffer-create "*request demo*")
                     (erase-buffer)
                     (insert (pp-to-string data))
                     (pop-to-buffer (current-buffer))))))
     :complete (lambda (&rest _) (message "Finished!"))
     :status-code '((400 . (lambda (&rest _) (message "Got 400.")))
                    (418 . (lambda (&rest _) (message "Got 418.")))))
    :ensure t
    :require t)

  (leaf macrostep
    :ensure t
    :bind (("C-c e" . macrostep-expand)))

  (leaf dashboard
    :ensure t
    :require t
    :custom ((dashboard-startup-banner . 4)
             (dashboard-items          . '((recents . 15)
		                           (projects . 5)
		                           (bookmarks . 5)
		                           (agenda . 5))))
    :hook (after-init . dashboard-setup-startup-hook)
    :config
    (add-to-list 'dashboard-items '(agenda) t))

  (leaf mwim
    :doc "Switch between the beginning/end of line or code"
    :ensure t
    :require t
    :bind (("C-a" . mwim-beginning-of-code-or-line)
           ("C-e" . mwim-end-of-code-or-line)))

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
                               "-dGraphicsAlphaBits=4" "-dQUIET")))))

(provide '30_utility)
;;; 30_utility.el ends here
