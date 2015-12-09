;;; 00_install.el ---

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/19 13:11:50>
;; Last-Updated: <2015/12/09 23:15:02>
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

;; auto install
(require 'cl)

(defvar installing-package-list
  '(
    ;; core-emacs
    solarized-theme
    init-loader

    ;; pos-tip utility
    pos-tip
    guide-key-tip
    
    ;; editor
    yasnippet
    yatemplate
    auto-complete
    
    ;; undo
    undohist
    undo-tree
    redo+
    
    ;; save emacs state
    session
    elscreen
    elscreen-persist

    ;; various utility
    minibuf-isearch
    mode-compile
    migemo
    fuzzy
    rainbow-mode
    shell-pop
    key-chord
    neotree
    sequential-command
    yascroll
    smartparens
    smartrep
    auto-install
    dired-rainbow
    tab-jump-out
    
    ;; additional major mode
    web-mode
    
    ;; helm
    helm
    helm-migemo
    helm-swoop
    ace-jump-mode
    ace-isearch
    ace-jump-helm-line
    
    ;; emacs lisp technique bible
    open-junk-file
    lispxmp
    paredit
    auto-async-byte-compile
    ))

(let ((not-installed
       (loop for package in installing-package-list
	     when (not (package-installed-p package))
	     collect package)))
  (when not-installed
    (package-refresh-contents)
    (dolist (pkg not-installed)
      (package-install pkg))))
