;;; 14_view-mode.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Keywords: 
;; Created:      <2015/12/03 03:23:28>
;; Last-Updated: <2015/12/03 03:25:27>

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

;; モードラインを派手に
(eval-after-load "view"
  '(setcar (cdr (assq 'view-mode minor-mode-alist))
           (if (fboundp 'propertize)
               (list (propertize " View"
                                 'face '(:foreground "white"
                                         :background "DeepPink1")))
             " View")))

;; viewmodeのキーバインドを優先
(add-hook 'view-mode-hook
          '(lambda ()
             (setq minor-mode-map-alist
                   (delete (assq 'view-mode minor-mode-map-alist)
                           minor-mode-map-alist)
                   minor-mode-map-alist
                   (cons (cons 'view-mode view-mode-map) minor-mode-map-alist))
             ))
