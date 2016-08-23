;;; init.el ---                                      -*- lexical-binding: t; -*-

;; Copyright (C) 2016  Naoya Yamashita

;; Author: Naoya Yamashita <conao@naoya-imac.local>
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

(require 'package)
(add-to-list 'package-archives '("melpa"     . "http://melpa.org/packages/") t)
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

(provide 'init)
;;; init.el ends here
