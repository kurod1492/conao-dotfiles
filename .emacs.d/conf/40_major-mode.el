;;; 40_major-mode.el ---                             -*- lexical-binding: t; -*-

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

(leaf org
  :config
  (leaf orglyth
    :config
    (leaf orglyth-html
      :config
      (setq orglyth-html-enable-option t
            orglyth-html-use-ftp       nil
            orglyth-html-local-root-path "~/Documents/sakura/orglyth"
            orglyth-html-remote-root-path "~/Documents/sakura/remote"
            orglyth-html-ftp-root-path    "/ftp:conao3@conao3.com:~/www/orglyth/")
      (orglyth-html-init)
      (orglyth-html-project-init))))

(provide '40_major-mode)
;;; 40_major-mode.el ends here
