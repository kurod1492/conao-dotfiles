;;; 98_hook.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 13:24:17>
;; Last-Updated: <2015/12/03 02:17:14>
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

(add-hook 'c++-mode (lambda () (add-to-list
                                'ac-sources
                                'ac-source-semantic)))
(add-hook 'c++-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c .") 'ac-complete-semantic)))

(add-hook 'emacs-lisp-mode-hook 'my-ac-emacs-lisp-setup)
(add-hook 'c-mode-common-hook 'my-ac-cc-mode-setup)
(add-hook 'css-mode-hook 'my-css-mode-setup)
(add-hook 'auto-complete-mode-hook 'my-ac-common-setup)

(add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
(add-hook 'css-mode-hook 'rainbow-mode)

;; shell-modeでpasswordを隠す
(add-hook 'comint-output-filter-functions 'comint-watch-for-password-prompt)

(add-hook 'after-init-hook #'global-flycheck-mode)
