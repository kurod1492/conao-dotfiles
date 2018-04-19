;;; 30_utility.el ---                                -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@naoya-imac.local>
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

(use-package other-window-or-split
  :init (el-get-bundle conao/other-window-or-split)
  :bind* (("C-t"   . other-window-or-split)
          ("C-S-t" . previous-other-window-or-split)
          ("M-t"   . split-window-dwim)
          ("C-c j" . adjust-windows-size))
  :config
  (setq split-window-width-with-em 100))

(provide '30_utility)
;;; 30_utility.el ends here
