;;; 00_leaf.el ---                                   -*- lexical-binding: t; -*-

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

;; (if (assoc 'leaf package-archive-contents)
;;     (package-install package)
;;   (package-refresh-contents)
;;   (package-install 'leaf))

;; support funcs
(defmacro add-list-to-list (dest-lst source-lst &optional append)
  "Add SOURCE_LST to DEST-LST in a destructive.
Defaltly, add at the beginning of the list,
but when APPEND is non-nil, SOURCE-LST is added at the end.
This function is minor change from `add-to-list'."
  (declare (indent 1))
  `(progn
     (mapc (lambda (x)
             (add-to-list ,dest-lst x ,append))
           (if ,append ,source-lst (reverse ,source-lst)))
     ,dest-lst))

(defmacro p (form)
  "Output expand given FORM."
  `(progn
     (pp (macroexpand-1 ',form))
     nil))

(defmacro po (form)
  "Output expand given FORM."
  `(progn
     (pp ,form)
     nil))

(eval
 `(add-list-to-list 'load-path
    ',(mapcar (lambda (x)
                (locate-user-emacs-file (format "site-lisp/%s" x)))
              '(;; depend packages
                "seml-mode.el" "cort-test.el"
                "leaf.el"

                ;; standalone packages
                "feather.el" "leaf-browser.el" "orglyth.el"

                ;; other vendor packages
                "straight.el" "po-mode.el"))))
(require 'leaf)

(leaf seml-mode
  :init (require 'seml-mode)
  :custom ((seml-live-refresh-interval     . 0.35)
           (seml-live-refresh-url-variable . ":type/:var1/:var2")
           (seml-live-refresh-url-quety    . '(targetpath targetfile))))
(leaf cort-test
  :init (require 'cort-test))
(leaf leaf
  :init (require 'leaf)
  :custom ((leaf-backend/:ensure . 'package)))
(leaf feather
  :init (require 'feather))
(leaf leaf-browser
  :init (require 'leaf-browser)
  :custom ((lbrowser-root-dir . "~/.emacs.d/site-lisp/leaf-browser.el/")
           (lbrowser-debugp . t))
  :config
  (leaf htmlize :ensure t)
  (leaf simple-httpd :ensure t
        :custom ((httpd-show-backtrace-when-error . t)))
  (leaf elquery :ensure t))
(leaf straight
  :init (require 'straight))
(leaf package
  :config
  (add-list-to-list 'package-archives
    '(("org"       . "https://orgmode.org/elpa/")
      ("melpa"     . "https://melpa.org/packages/")
      ("marmalade" . "https://marmalade-repo.org/packages/")))
  (package-initialize))

(leaf bind-key :ensure t)

(leaf s    :ensure t)
(leaf f    :ensure t)
(leaf dash :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Reference packages
;;

(leaf use-package :ensure t)

(use-package el-get :ensure t
  :if (or (executable-find "git")
          (message "'git' couldn't found. el-get can't download any packages")
          (defmacro el-get (&rest arg) nil))
  :config
  (setq el-get-git-shallow-clone  t
        el-get-emacswiki-base-url "http://www.emacswiki.org/emacs/download/"))

(leaf po-mode.el
  :require po-mode po-compat
  :config
  (autoload 'po-mode "po-mode"
    "Major mode for translators when they edit PO files.

Special commands:
\\{po-mode-map}
Turning on PO mode calls the value of the variable 'po-mode-hook',
if that value is non-nil.  Behaviour may be adjusted through some variables,
all reachable through 'M-x customize', in group 'Emacs.Editing.I18n.Po'."
    t)
  (setq auto-mode-alist
        (cons '("\\.po\\'\\|\\.po\\." . po-mode) auto-mode-alist))

  ;; For viewing PO and POT files.

  ;; To use the right coding system automatically under Emacs 20 or newer.
  (unless (fboundp 'po-find-file-coding-system)
    (autoload 'po-find-file-coding-system "po-compat"
      "\
Return a Mule (DECODING . ENCODING) pair, according to PO file charset.
Called through file-coding-system-alist, before the file is visited for real."))
  (modify-coding-system-alist 'file "\\.po[t]?\\'\\|\\.po\\."
                              'po-find-file-coding-system))

(provide '00_leaf)
;;; 00_leaf.el ends here
