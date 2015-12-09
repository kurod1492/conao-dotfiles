;;; 09_newcomment.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/20 14:27:01>
;; Last-Updated: <2015/11/30 16:57:46>
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

;; http://d.hatena.ne.jp/tomoya/20091015/1255593575

;; transient-mark-mode on
(setq-default transient-mark-mode t)
(setq comment-style 'multiline)

;; transient-mark-mode がオンでリージョンが有効のときに M-; すると、コメントアウト、もしくは解除のコマンドになる
;; transient-mark-mode がオンでリージョンが有効のときに C-u 数値 M-; すると、コメント文字列を数値分にする(下に補足説明あり)
;; 何もない行で M-; した場合、コメント文字列を挿入する
;; 何か書かれている行で M-; した場合、行末にコメント文字列を挿入する
;; コメント行で M-; した場合、コメント文までジャンプする
;; コメント行で、引数を与えて M- ; した場合(C-u M-; という感じ)、コメント行であれば削除する。

;; M-; runs the command comment-dwim, which is an interactive compiled
;; Lisp function in `newcomment.el'.

;; It is bound to M-;.

;; (comment-dwim ARG)

;; Call the comment command you want (Do What I Mean).
;; If the region is active and `transient-mark-mode' is on, call
;; `comment-region' (unless it only consists of comments, in which
;; case it calls `uncomment-region').
;; Else, if the current line is empty, call `comment-insert-comment-function'
;; if it is defined, otherwise insert a comment and indent it.
;; Else if a prefix ARG is specified, call `comment-kill'.
;; Else, call `comment-indent'.
;; You can configure `comment-style' to change the way regions are commented.

;; (defconst comment-styles
;;   '((plain	. (nil nil nil nil))
;;     (indent	. (nil nil nil t))
;;     (indent-or-triple
;;                 . (nil nil nil multi-char))
;;     (aligned	. (nil t nil t))
;;     (multi-line	. (t nil nil t))
;;     (extra-line	. (t nil t t))
;;     (box	. (nil t t t))
;;     (box-multi	. (t t t t)))
