;;; 42_swap.el --- 

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Keywords: 
;; Created:      <2015/12/03 03:13:04>
;; Last-Updated: <2015/12/03 19:44:00>

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
(require 'swap)

;; swap-rectangle

;; とある二つの rectangle をいれかえます.
;; たとえば
;; A I X
;; B J Y
;; C K Z
;; という状態から,
;; X I A
;; Y J B
;; Z K C
;; みたいに変更したい場合, レジスタを使ったり kill-rectangle を使ったりする方法だと ちょっと余計な手間がかかりますよね.
;; この程度のことを一発でできる elisp くらい既にありそうなもんですが, 私の調べた限 りでは見付からなかったのでした.
;; 結構こういう処理をしたいことが多かったので, 作ってみました.
;; 使い方は,
;; M-x swap-rectangle
;; いれかえたい片方の rectangle の開始位置で C-SPC
;; いれかえたい片方の rectangle の終了位置で Enter(ここまでがハイライトされます)
;; いれかえたいもう一方の rectangle の開始位置で C-SPC
;; いれかえたいもう一方の rectangle の終了位置で Enter
;; とするだけです.
;; やめるときは C-g で.
;; たぶんやってみればすぐわかるかと.


;; swap-region

;; 二つの領域をいれかえます.
;; まあ emacs なんだから yank-pop 使えばちょっとの手間でできますけど, せっかくだか ら region をいれかえるのも作ってみたわけです.
;; 使い方は,
;; M-x swap-region
;; いれかえたい片方の region の開始位置で C-SPC
;; いれかえたい片方の region の終了位置で Enter
;; いれかえたいもう一方の region の開始位置で C-SPC
;; いれかえたいもう一方の region の終了位置で Enter
;; とするだけです.
;; やめるときは C-g で.
;; たぶんやってみればすぐわかるかと.
