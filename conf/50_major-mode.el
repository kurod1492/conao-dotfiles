;;; 50_major-mode.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:25>
;; Last-Updated: <2015/12/21 15:38:31>
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
(use-package web-mode
  :ensure t
  :defer  t
  :mode  (("\\.html?\\'" . web-mode)
          ("\\.jsp\\'"   . web-mode)
          ("\\.gsp\\'"   . web-mode))
  :config (setq web-mode-markup-indent-offset 4
                web-mode-css-indent-offset 4
                web-mode-code-indent-offset 4))

(use-package yatex
  :ensure t
  :defer  t
  :mode (("\\.tex" . yatex-mode)))
