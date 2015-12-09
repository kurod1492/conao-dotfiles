;;; split-window.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/29 16:04:04>
;; Last-Updated: <2015/12/10 05:48:32>
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
;;; calc window size from frame size
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))

;;; window change and separate frame with C-t
(defun other-window-or-split ()
  (interactive)
  (when (one-window-p)
    (if (<= (window-body-height) 50)
		(split-window-vertically)
      
      (if (<= (window-body-width) 270)
		  (split-window-horizontally)
		(split-window-horizontally-n 3))))
  (other-window 1))

;;; split window absolutely
(defun split-window-suitably ()
  (interactive)
  (if (<= (window-body-height) 50)
	  (if (<= (window-body-width) 150)
		  (split-window-vertically))
	(if (<= 270 (window-body-width))
		(split-window-horizontally-n 3)
	  (if (<= (* (window-body-height) 2) (window-body-width))
		  (split-window-horizontally)
		(split-window-vertically))
	  (other-window 1))))

(provide 'split-window)
;;; split-window.el ends here
