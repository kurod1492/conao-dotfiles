;;; 00_use-pacakge.El --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/14 10:35:27>
;; Last-Updated: <2015/12/22 09:45:18>
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

(when (not (package-installed-p 'use-package))
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)

;; key-chord
(use-package key-chord
  :ensure t
  :config (key-chord-mode 1))

(use-package use-package-chords
  :ensure t)

(use-package smartrep
  :ensure t)

(defsubst hook-into-modes (func &rest modes)
  (dolist (mode-hook modes) (add-hook mode-hook func)))


;; (use-package autopair
;;   :disabled t
;;   :load-path "site-lisp/autopair"
;;   :commands autopair-mode
;;   :diminish autopair-mode
;;   :init
;;   (hook-into-modes #'autopair-mode
;;                    'c-mode-common-hook
;;                    'text-mode-hook
;;                    'ruby-mode-hook
;;                    'python-mode-hook
;;                    'sh-mode-hook))
