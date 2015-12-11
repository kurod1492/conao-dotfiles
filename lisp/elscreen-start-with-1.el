;;; elscreen-start-with-1.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/12/11 14:32:12>
;; Last-Updated: <2015/12/11 14:33:30>
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
;; 既存スクリーンのリストを要求された際、0 番が存在しているかのように偽装する
(defadvice elscreen-get-screen-list (after my-ad-elscreen-get-screenlist disable)
  (add-to-list 'ad-return-value 0))

;; スクリーン生成時に 0 番が作られないようにする
(defadvice elscreen-create (around my-ad-elscreen-create activate)
  (interactive)
  ;; 0 番が存在しているかのように偽装
  (ad-enable-advice 'elscreen-get-screen-list 'after 'my-ad-elscreen-get-screenlist)
  (ad-activate 'elscreen-get-screen-list)
  ;; 新規スクリーン生成
  ad-do-it
  ;; 偽装解除
  (ad-disable-advice 'elscreen-get-screen-list 'after 'my-ad-elscreen-get-screenlist)
  (ad-activate 'elscreen-get-screen-list))

;; スクリーン 1 番を作成し 0 番を削除 (起動時、フレーム生成時用)
(defun my-elscreen-kill-0 ()
  (when (and (elscreen-one-screen-p)
             (elscreen-screen-live-p 0))
    (elscreen-create)
    (elscreen-kill 0)))

;; フレーム生成時のスクリーン番号が 1 番になるように
(defadvice elscreen-make-frame-confs (after my-ad-elscreen-make-frame-confs activate)
  (let ((selected-frame (selected-frame)))
    (select-frame frame)
    (my-elscreen-kill-0)
    (select-frame selected-frame)))

(provide elscreen-start-with-1)
;;; elscreen-start-with-1.el ends here
