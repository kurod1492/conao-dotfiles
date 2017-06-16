;;; 00_use-package.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017 Naoya Yamashita

;; Author: Naoya Yamashita
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

;; config packages
(use-package key-chord          :ensure t :config (key-chord-mode 1))
(use-package use-package-chords :ensure t)
(use-package smartrep           :ensure t)
(use-package el-get             :ensure t)

(defsubst hook-into-modes (func &rest modes)
  "Apply function to hook. (add-hook MODES FUNC).
Use like that
(hook-into-modes 'hl-line-mode '(prog-mode-hook
                                 package-menu-mode-hook))"
  (dolist (mode-hook modes)
    (add-hook mode-hook func)))

(eval-when-compile
  (require 'use-package)
  (require 'diminish)
  (require 'bind-key))

(provide '00_use-package)
;;; 00_use-package.el ends here
