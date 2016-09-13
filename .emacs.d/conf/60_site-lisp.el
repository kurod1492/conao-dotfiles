;;; site-lisp.el

;;; Code:
(use-package auto-save-buffers
  :config (progn
            (run-with-idle-timer 0.5 t 'auto-save-buffers)
            (bind-key "C-x a s" 'auto-save-buffers-toggle)))

(use-package navbarx-elscreen)
(use-package navbarx-version)
(use-package navbarx-time
  :config (progn
            (setq navbar-item-list '(navbarx-version
                                     navbarx-time
                                     navbarx-elscreen))
            (navbar-mode)
            (navbar-revive-workaround)
            (display-time-mode)

            ;;: elscreen-start ;;;
            (elscreen-start)))

(use-package ox-qmd
  :config (progn
            (add-to-list 'ox-qmd-language-keyword-alist '("shell-script" . "sh"))))

(use-package edit-list)

(use-package swap
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
  ;; この程度のことを一発でできる elisp くらい既にありそうなもんですが, 私の調べた限りでは見付からなかったのでした.
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

  :commands swap-region swap-rectangle)
(use-package visual-basic-mode
  :config
  (setq visual-basic-mode-indent 4))

(use-package multiple-cursors
  :config
  (use-package smartrep
    :config (declare-function smartrep-define-key "smartrep"))

  (global-set-key (kbd "C-M-c") 'mc/edit-lines)
  (global-set-key (kbd "C-M-r") 'mc/mark-all-in-region)

  (global-unset-key "\C-t")

  (smartrep-define-key global-map "C-t"
    '(("C-t"      . 'mc/mark-next-like-this)
      ("n"        . 'mc/mark-next-like-this)
      ("p"        . 'mc/mark-previous-like-this)
      ("m"        . 'mc/mark-more-like-this-extended)
      ("u"        . 'mc/unmark-next-like-this)
      ("U"        . 'mc/unmark-previous-like-this)
      ("s"        . 'mc/skip-to-next-like-this)
      ("S"        . 'mc/skip-to-previous-like-this)
      ("*"        . 'mc/mark-all-like-this)
      ("d"        . 'mc/mark-all-like-this-dwim)
      ("i"        . 'mc/insert-numbers)
      ("o"        . 'mc/sort-regions)
      ("O"        . 'mc/reverse-regions))))
