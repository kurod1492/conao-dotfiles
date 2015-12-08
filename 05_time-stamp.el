;;; 06_time-stamp.el --- insert time-stamp

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/19 14:37:29>
;; Last-Updated: <2015/12/03 21:28:12>
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

;; 最終更新日の自動挿入
(require 'time-stamp)
(add-hook 'before-save-hook 'time-stamp)
(setq time-stamp-active t)
(setq time-stamp-start "[lL]ast[ -][uU]pdated[ \t]*:[ \t]*<")
(setq time-stamp-format "%:y/%02m/%02d %02H:%02M:%02S")
(setq time-stamp-end ">")
(setq time-stamp-line-limit 20)

;; %:a -- Monday 曜日
;; %#A -- MONDAY 全部大文字で曜日
;; %:b -- January 月
 
;; 桁数を指定すると指定した文字だけが表示される.
;; "%2#A"なら MO など．
 
;; %02H -- 15  時刻 (24 時間)
;; %02I -- 03  時刻 (12 時間)
;; %#p  -- pm  PM と AM の別
;; %P   -- PM  PM と AM の別
;; %w   -- 土曜なら 6. 日曜を 0 とし，何番目の曜日なのか
;; %02y -- 03  西暦の下 2 桁．
;; %z   -- jst  タイムゾーン
;; %Z   -- JST  タイムゾーン
;; %%   -- %自体を入力
;; %f   -- ファイル名
;; %F   -- ファイル名のフルパス
;; %s   -- マシン名
;; %u   -- ログインしたユーザ名
;; %U   -- ログインしたユーザのフルネーム 
