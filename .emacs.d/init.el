
;; if you run like 'emacs -q -l ~/hoge/init.el'
;; load settings in ~/hoge/
(if load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name))
  (setq user-emacs-directory "~/.emacs.d/"))

(defun add-to-load-path (&rest paths)
  (let (path)
	(dolist (path paths paths)
	  (let ((default-directory
			  (expand-file-name (concat user-emacs-directory path))))
		(add-to-list 'load-path default-directory)
		(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
			(normal-top-level-add-subdirs-to-load-path))))))

(setq load-path-folder-list '("site-lisp" "conf" "elpa" "el-get"))

(dolist (folder load-path-folder-list)
  (unless (file-directory-p (concat user-emacs-directory folder))
    (mkdir (concat user-emacs-directory folder))
    (message "mkdir: %s%s" user-emacs-directory folder))
  (add-to-load-path folder))
(add-to-load-path "site-lisp" "conf")

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

(defmacro user-setting-directory (directory)
  (concat user-emacs-directory directory))

(init-loader-load (user-setting-directory "conf"))
