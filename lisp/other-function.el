;;; other-function.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/12 08:28:58>
;; Last-Updated: <2015/12/12 08:29:09>
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
(defun region-or-prompt (prompt)
  (if (region-active-p)
      (prog1
          (buffer-substring (region-beginning) (region-end))
        (deactivate-mark)
        (message ""))
    (read-string (format "%s (default:%s): " prompt (thing-at-point 'symbol))
                 nil nil (thing-at-point 'symbol))))
