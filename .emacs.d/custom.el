;;; custom.el ---                                    -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Naoya Yamashita

;; Author: Naoya Yamashita <conao@Naoya-no-MacBook-Air.local>
;; Keywords: convenience

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



(provide 'custom)
;;; custom.el ends here

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elscreen-prefix-key (kbd "C-c e"))
 '(elscreen-tab-display-control nil)
 '(elscreen-tab-display-kill-screen nil)
 '(flycheck-display-errors-function (function flycheck-pos-tip-error-messages))
 '(flycheck-keymap-prefix (kbd "C-c f"))
 '(package-selected-packages
   (quote
    (elisp-slime-nav anzu yatemplate yascroll use-package-chords undohist undo-tree solarized-theme smartrep shell-pop session sequential-command recentf-ext rainbow-mode popwin pdf-tools paredit org2blog open-junk-file mode-compile minibuf-isearch magit lispxmp init-loader htmlize helm google-translate fuzzy free-keys fold-dwim flycheck-pos-tip flex-autopair elscreen-persist el-get dired-subtree dired-rainbow dired-filter dired-details buttercup auto-install auto-complete))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
