;;; 40_helm.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/10 05:38:13>
;; Last-Updated: <2015/12/13 10:47:43>
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
(use-package helm
  :ensure t
  :defer  t 
  :config (progn
            (helm-mode t)
            (setq helm-display-function
                  (lambda (buf)
                    (split-window-vertically)
                    (other-window 1)
                    (switch-to-buffer buf)))
            (defadvice helm-ff-kill-or-find-buffer-fname (around execute-only-if-exist activate)
              "Execute command only if CANDIDATE exists"
              (when (file-exists-p candidate)
                ad-do-it))
            (bind-keys :map helm-map
                       ("C-h" . delete-backward-char))
            (bind-keys :map helm-find-files-map
                       ("C-h" . delete-backward-char)
                       ("TAB" . helm-execute-persistent-action))
            (bind-keys :map helm-read-file-map
                       ("TAB" . helm-execute-persistent-action)))
  :bind (("M-x"    . helm-M-x)
         ("C-x C-f". helm-find-files)
         ("C-x C-r". helm-recentf)
         ("M-y"    . helm-show-kill-ring)
         ("C-c i"  . helm-imenu)
         ("C-x b"  . helm-buffers-list)))

(use-package helm-migemo
  :disabled t
  :if (executable-find "cmigemo")
  :ensure helm
  ;; :defer  t
  :config (defun helm-compile-source--candidates-in-buffer (source)
            (helm-aif (assoc 'candidates-in-buffer source)
                (append source
                        `((candidates
                           . ,(or (cdr it)
                                  (lambda ()
                                    ;; Do not use `source' because other plugins
                                    ;; (such as helm-migemo) may change it
                                    (helm-candidates-in-buffer (helm-get-current-source)))))
                          (volatile) (match identity)))
              source))
  ;; [2015-09-06 Sun]helm-match-plugin -> helm-multi-match変更の煽りを受けて
  (defalias 'helm-mp-3-get-patterns 'helm-mm-3-get-patterns)
  (defalias 'helm-mp-3-search-base 'helm-mm-3-search-base))

(use-package ace-jump-mode
  :ensure helm
  :defer  t)

(use-package helm-swoop
  :ensure helm
  :defer  t)

(use-package ace-isearch
  :ensure helm
  ;; :defer  t
  ;; 1文字     -> ace-jump-mode
  ;; 2〜5文字  -> isearch
  ;; 6文字以上 -> helm-swoop
  :config (global-ace-isearch-mode))
