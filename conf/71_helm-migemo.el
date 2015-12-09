;;; 71_helm-migemo.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 15:30:17>
;; Last-Updated: <2015/11/30 17:20:19>
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

(require 'helm-migemo)
;;; この修正が必要
(with-eval-after-load "helm-migemo"
  (defun helm-compile-source--candidates-in-buffer (source)
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
