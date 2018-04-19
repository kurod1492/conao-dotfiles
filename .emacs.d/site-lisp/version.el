;;; version.el ---                                   -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@184-187.cup.hiroshima-u.ac.jp>
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

;; http://d.hatena.ne.jp/tomoya/20090807/1249601308
(defun x->bool (elt)
  "Bool ELT."
  (not (not elt)))

(defvar emacs22-p (= 22 emacs-major-version))
(defvar emacs23-p (= 23 emacs-major-version))
(defvar emacs24-p (= 24 emacs-major-version))
(defvar emacs25-p (= 25 emacs-major-version))

;; less than ~
(defvar emacs22-l-p (>= 22 emacs-major-version))
(defvar emacs23-l-p (>= 23 emacs-major-version))
(defvar emacs24-l-p (>= 24 emacs-major-version))
(defvar emacs25-l-p (>= 25 emacs-major-version))

;; greater than ~
(defvar emacs22-g-p (<= 22 emacs-major-version))
(defvar emacs23-g-p (<= 23 emacs-major-version))
(defvar emacs24-g-p (<= 24 emacs-major-version))
(defvar emacs25-g-p (<= 25 emacs-major-version))

(defvar emacs23.0-p (string-match "^23\.0" emacs-version))
(defvar emacs23.1-p (string-match "^23\.1" emacs-version))
(defvar emacs23.2-p (string-match "^23\.2" emacs-version))
(defvar emacs23.3-p (string-match "^23\.3" emacs-version))
(defvar emacs23.4-p (string-match "^23\.4" emacs-version))
(defvar emacs24.1-p (string-match "^24\.1" emacs-version))
(defvar emacs24.2-p (string-match "^24\.2" emacs-version))
(defvar emacs24.3-p (string-match "^24\.3" emacs-version))
(defvar emacs24.4-p (string-match "^24\.4" emacs-version))
(defvar emacs24.5-p (string-match "^24\.5" emacs-version))
(defvar emacs25.0-p (string-match "^25\.0" emacs-version))
(defvar emacs-bzr-p (string-match "^2.\..\.50" emacs-version))

(defvar darwin-p  (eq system-type 'darwin))
(defvar ns-p      (eq window-system 'ns))
(defvar carbon-p  (eq window-system 'mac))
(defvar linux-p   (eq system-type 'gnu/linux))
(defvar colinux-p (when linux-p
                    (let ((file "/proc/modules"))
                      (and
                       (file-readable-p file)
                       (x->bool
                        (with-temp-buffer
                          (insert-file-contents file)
                          (goto-char (point-min))
                          (re-search-forward "^cofuse\.+" nil t)))))))
(defvar cygwin-p  (eq system-type 'cygwin))
(defvar nt-p      (eq system-type 'windows-nt))
(defvar meadow-p  (featurep 'meadow))
(defvar windows-p (or cygwin-p nt-p meadow-p))
(defvar nox-p     (eq window-system nil))

(provide 'version)
;;; version.el ends here

