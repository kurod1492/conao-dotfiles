
;; if you run like 'emacs -q -l ~/hoge/init.el'
;; load settings in ~/hoge/
(when load-file-name
  (setq user-emacs-directory (file-name-directory load-file-name)))

(require 'package)
(add-to-list 'package-archives '("gnu"       . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org"       . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;; theme settings
(unless (package-installed-p 'solarized-theme)
  (package-refresh-contents)
  (package-install 'solarized-theme))
(load-theme 'solarized-dark t)

;; (unless (package-installed-p 'dracula-theme)
;;   (package-refresh-contents)
;;   (package-install 'dracula-theme))
;; (load-theme 'dracula t)

;; init-loader
(unless (package-installed-p 'init-loader)
  (package-refresh-contents)
  (package-install 'init-loader))
(require 'init-loader)
;; (custom-set-variables
;;  '(init-loader-show-log-after-init 'error-only))
(init-loader-load "~/.emacs.d/conf")
