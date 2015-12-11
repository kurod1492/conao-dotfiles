;;; view-next-file.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Keywords: 
;; Created:      <2015/12/03 03:08:00>
;; Last-Updated: <2015/12/11 12:09:09>

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
;; http://homepage1.nifty.com/blankspace/emacs/dired.html

(defvar conao-view-from-dired-flag nil)
(make-variable-buffer-local 'conao-view-from-dired-flag)
(defvar conao-binary-file-ext-list
  '("tar" "tar.gz" "mpg" "jpg" "gif" "png"))
(defvar conao-binary-file-exp "\\.elc$")
(let ((l conao-binary-file-ext-list))
  (while l
    (setq conao-binary-file-exp
          (concat conao-binary-file-exp "\\|\\." (car l) "$"))
    (setq l (cdr l))))
(defun conao-dired-view-file ()
  (interactive)
  (unless (or (file-directory-p (dired-get-filename))
              (string-match conao-binary-file-exp (dired-get-filename)))
    (view-file (dired-get-filename))
    (setq conao-view-from-dired-flag t)))

(defun conao-view-next-file-in-dired ()
  (interactive)
  (when conao-view-from-dired-flag
    (View-kill-and-leave)
    (conao-dired-next-line 1)
    (conao-dired-view-file)))

(defun conao-view-prev-file-in-dired ()
  (interactive)
  (when conao-view-from-dired-flag
    (View-kill-and-leave)
    (conao-dired-previous-line 1)
    (conao-dired-view-file)))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "\C-v"      'conao-dired-view-file)
             ))
(add-hook 'view-mode-hook
          '(lambda ()
             (define-key view-mode-map "N" 'conao-view-next-file-in-dired)
             (define-key view-mode-map "P" 'conao-view-prev-file-in-dired)
             ))

(provide 'view-next-file)
;;; view-next-file.el ends here
