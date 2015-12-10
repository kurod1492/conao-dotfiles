;;; elscreen-swap.el ---

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/29 16:05:40>
;; Last-Updated: <2015/12/10 05:57:07>
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
(require 'elscreen)

(defun elscreen-swap-previous()
  "Interchange screens selected currently and previous."
  (interactive)
  (cond
   ((elscreen-one-screen-p)
    (elscreen-message "There is only one screen, cannot swap"))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) '>))
           (previous-screen
            (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
                (car screen-list)))
           (current-screen (elscreen-get-current-screen))
           (current-screen-property
            (elscreen-get-screen-property current-screen))
           (previous-screen-property
            (elscreen-get-screen-property previous-screen)))
      (elscreen-set-screen-property current-screen previous-screen-property)
      (elscreen-set-screen-property previous-screen current-screen-property)
      (elscreen-goto-internal (elscreen-get-current-screen)))))
  (elscreen-previous))

(defun elscreen-swap-next()
  "Interchange screens selected currently and next."
  (interactive)
  (cond
   ((elscreen-one-screen-p)
    (elscreen-message "There is only one screen, cannot swap"))
   (t
    (let* ((screen-list (sort (elscreen-get-screen-list) '<))
           (next-screen
            (or (nth 1 (memq (elscreen-get-current-screen) screen-list))
                (car screen-list)))
           (current-screen (elscreen-get-current-screen))
           (current-screen-property
            (elscreen-get-screen-property current-screen))
           (next-screen-property
            (elscreen-get-screen-property next-screen)))
      (elscreen-set-screen-property current-screen next-screen-property)
      (elscreen-set-screen-property next-screen current-screen-property)
      (elscreen-goto-internal (elscreen-get-current-screen)))))
  (elscreen-next))

(provide 'elscreen-swap)
;;; elscreen-swap.el ends here
