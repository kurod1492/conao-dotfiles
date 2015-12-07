;;; 84_view-next-file.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Keywords: 
;; Created:      <2015/12/03 03:08:00>
;; Last-Updated: <2015/12/03 03:08:20>

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

(defvar my-view-from-dired-flag nil)
(make-variable-buffer-local 'my-view-from-dired-flag)
(defvar my-binary-file-ext-list
  '("tar" "tar.gz" "mpg" "jpg" "gif" "png"))
(defvar my-binary-file-exp "\\.elc$")
(let ((l my-binary-file-ext-list))
  (while l
    (setq my-binary-file-exp
          (concat my-binary-file-exp "\\|\\." (car l) "$"))
    (setq l (cdr l))))
(defun my-dired-view-file ()
  (interactive)
  (unless (or (file-directory-p (dired-get-filename))
              (string-match my-binary-file-exp (dired-get-filename)))
    (view-file (dired-get-filename))
    (setq my-view-from-dired-flag t)))

(defun my-View-next-file-in-dired ()
  (interactive)
  (when my-view-from-dired-flag
    (View-kill-and-leave)
    (my-dired-next-line 1)
    (my-dired-view-file)))

(defun my-View-prev-file-in-dired ()
  (interactive)
  (when my-view-from-dired-flag
    (View-kill-and-leave)
    (my-dired-previous-line 1)
    (my-dired-view-file)))

(add-hook 'dired-mode-hook
          '(lambda ()
             (define-key dired-mode-map "\C-v"      'my-dired-view-file)
             ))
(add-hook 'view-mode-hook
          '(lambda ()
             (define-key view-mode-map "N" 'my-View-next-file-in-dired)
             (define-key view-mode-map "P" 'my-View-prev-file-in-dired)
             ))
