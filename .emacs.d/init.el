;;; init.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Naoya Yamashita

;; Author: Naoya Yamashita <conao@Naoya-MacBook-Air.local>
;; Keywords:

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

;; if you run like 'emacs -q -l ~/hoge/init.el'
;; load settings in ~/hoge/
(if load-file-name
    (setq user-emacs-directory (file-name-directory load-file-name))
  (setq user-emacs-directory "~/.emacs.d/"))

(defmacro user-setting-directory (directory)
  "Return user-emacs-directory/DIRECTORY to setting Emacs."
  (concat user-emacs-directory directory))

(defun add-to-load-path (&rest paths)
  "Add load path recursive in PATHS."
	(dolist (path paths paths)
	  (let ((default-directory
			  (expand-file-name (concat user-emacs-directory path))))
		(add-to-list 'load-path default-directory)
		(if (fboundp 'normal-top-level-add-subdirs-to-load-path)
			(normal-top-level-add-subdirs-to-load-path)))))

(defvar load-path-folder-list '("site-lisp" "conf" "elpa" "el-get" "auto-install"))

(dolist (folder load-path-folder-list)
  (unless (file-directory-p (concat user-emacs-directory folder))
    (mkdir (concat user-emacs-directory folder))
    (message "mkdir: %s%s" user-emacs-directory folder))
  (add-to-load-path folder))

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

(init-loader-load (user-setting-directory "conf"))

(provide 'init)
;;; init.el ends here
