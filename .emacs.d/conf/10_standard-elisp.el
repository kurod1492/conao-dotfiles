;;; 10_standard-elisp.el ---                         -*- lexical-binding: t; -*-

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

(leaf paren
  :config
  (setq show-paren-delay 0.0)
  (show-paren-mode 1))

(leaf cus-edit
  :config
  (setq custom-file (locate-user-emacs-file "custom.el")))

(provide '10_standard-elisp)
;;; 10_standard-elisp.el ends here
