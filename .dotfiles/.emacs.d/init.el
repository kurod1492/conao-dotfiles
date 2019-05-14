;;; init.el ---                                      -*- lexical-binding: t; -*-

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

;;; Code:


(prog1
  "Change user-emacs-directory"
  ;; enable debug
  (setq debug-on-error  t
        init-file-debug t)

  ;; you can run like 'emacs -q -l ~/hoge/init.el'
  (when load-file-name
    (setq user-emacs-directory
          (expand-file-name (file-name-directory load-file-name))))

  ;; change user-emacs-directory
  (setq user-emacs-directory
        (expand-file-name
         (format "local/%s.%s/"
                 emacs-major-version emacs-minor-version)
         user-emacs-directory))
  (make-directory user-emacs-directory t))

(prog1
  "Load leaf.el"
  (add-to-list 'load-path (locate-user-emacs-file "site-lisp/leaf.el"))
  (require 'leaf)
  (leaf leaf
    :doc "Symplify your init.el configuration"
    :doc "Initialize leaf dependent packages"
    :custom ((leaf-backend-ensure . 'package)
             (leaf-backend-bind   . 'bind-key))
    :config
    (leaf package
      :custom ((package-archives . '(("org"   . "https://orgmode.org/elpa/")
                                     ("melpa" . "https://melpa.org/packages/")
                                     ("gnu"   . "https://elpa.gnu.org/packages/"))))
      :config
      (package-initialize))
    (leaf bind-key :ensure t)))


(leaf initialize-emacs
  :config
  (leaf exec-path-from-shell
    :ensure t
    :when (memq '(mac ns x) window-system)
    :custom ((exec-path-from-shell-check-startup-files . nil)
             (exec-path-from-shell-variables . '("PATH" "GOPATH")))
    :config
    (exec-path-from-shell-initialize))

  (global-unset-key (kbd "M-o"))
  (global-unset-key (kbd "M-t")))


(leaf conao3-packages
  :doc "Elisp packages are developed by conao3"
  :config
  (leaf melpa-packages
    :config
    (leaf seml-mode
      :ensure t
      :custom ((seml-live-refresh-interval     . 0.35)
               (seml-live-refresh-url-variable . ":type/:var1/:var2")
               (seml-live-refresh-url-quety    . '(targetpath targetfile)))))

  (leaf site-lisp-packages
    :config
    (leaf cort-test
      :load-path `,(locate-user-emacs-file "site-lisp/cort-test.el")
      :require t)

    (leaf feather
      :load-path `,(locate-user-emacs-file "site-lisp/feather.el")
      :require t)

    (leaf feather-server
      :load-path `,(locate-user-emacs-file "site-lisp/feather-server.el")
      :require t)

    (leaf leaf-browser
      :load-path `,(locate-user-emacs-file "site-lisp/leaf-browser.el")
      :require t
      :custom ((lbrowser-root-dir . "~/.emacs.d/site-lisp/leaf-browser.el/")
               (lbrowser-debugp   . t))
      :config
      (leaf htmlize :ensure t)
      (leaf simple-httpd
        :ensure t
        :custom ((httpd-show-backtrace-when-error . t))))

    (leaf navbar
      :load-path `,(locate-user-emacs-file "site-lisp/navbar.el")
      :require t)

    (leaf orglyth
      :load-path `,(locate-user-emacs-file "site-lisp/orglyth.el")
      :require t orglyth-html orglyth-latex)))

(provide 'init)
;;; init.el ends here
