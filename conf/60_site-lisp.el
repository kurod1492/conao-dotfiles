;;; 60_site-lisp.Eli --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:37>
;; Last-Updated: <2015/12/11 14:17:06>
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
(use-package auto-save-buffers
  :config (run-with-idle-timer 0.5 t 'auto-save-buffers)
  :bind   (("C-x a s" . auto-save-buffers-toggle)))

(use-package navbarx-elscreen)
(use-package navbarx-version)
(use-package navbarx-time
  :config (progn
            (setq navbar-item-list '(navbarx-version
                                   navbarx-time
                                   navbarx-elscreen))
            (navbar-mode)
            (navbar-revive-workaround)
            (display-time-mode)

            ;;: elscreen-start ;;;
            (elscreen-start)))
