;;; init.el is first eval init file.
;;; this file is use for ONLY package and init-loader and theme

;;; package manager
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;; theme settings
(unless (package-installed-p 'solarized-theme)
  (package-refresh-contents)
  (package-install 'solarized-theme))
(load-theme 'solarized-dark t)

;;; init-loader
(unless (package-installed-p 'init-loader)
  (package-refresh-contents)
  (package-install 'init-loader))
(require 'init-loader)
;; (custom-set-variables
;;  '(init-loader-show-log-after-init 'error-only))
(init-loader-load "~/.emacs.d/conf")
