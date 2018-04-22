;;; 10_standard-elisp.el ---                         -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita <conao@naoya-imac.local>
;; Keywords: .emacs

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

(use-package windmove
  :bind (("C-c <left>" . windmove-left)
         ("C-c <down>"  . windmove-down)
         ("C-c <up>"    . windmove-up)
         ("C-c <right>" . windmove-right)))

(use-package cus-edit :defer t
  :config
  (setq custom-file (user-setting-directory "custom.el"))
  (when (file-exists-p custom-file)
    (load custom-file)))

(use-package battery
  :config
  (display-battery-mode t))

(use-package generic-x
  :config
  (global-font-lock-mode t)
  ;; hi-light 'FIXME:' in c-source file
  (add-hook 'c-mode-hook
            (lambda ()
              (font-lock-add-keywords
               nil
               '(("\\<\\(FIXME\\):" 1
                  font-lock-warning-face t))))))

(use-package time-stamp
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
  :config
  (add-hook 'before-save-hook 'time-stamp)
  (setq time-stamp-active t
        time-stamp-start "[lL]ast[ -][uU]pdated[ \t]*:[ \t]*<"
        time-stamp-format "%:y/%02m/%02d"
        time-stamp-end ">"
        time-stamp-line-limit 20))

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets))

(use-package linum
  :config
  (global-linum-mode t)
  (setq linum-format "%5d")
  (set-face-attribute 'linum nil :height 120))

(use-package newcomment
  :config
  (setq-default transient-mark-mode t)
  (setq comment-style 'multiline))

(use-package Flyspell
  :bind* (("<f12>" . flyspell-mode)
          ("<f10>" . flyspell-buffer)
          ("<f9>"  . ispell-word))
  :config
  (setq-default ispell-program-name "aspell")

  ;; fly-spell
  (mapc
   (lambda (hook)
     (add-hook hook 'flyspell-prog-mode))
   '(
     c++-mode-hook
     emacs-lisp-mode-hook
     ruby-mode-hook
     python-mode-hook
     ))
  (mapc
   (lambda (hook)
     (add-hook hook
               '(lambda () (flyspell-mode 1))))
   '(
     fundamental-mode
     text-mode-hook
     org-mode-hook
     yatex-mode-hook
     ))
  (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))

(use-package view
  :config
  (prog1 "unwritable file, open in view-mode"
    ;; unwritable file, open in view mode
    (defadvice find-file
        (around find-file-switch-to-view-file (file &optional wild) activate)
      (if (and (not (file-writable-p file))
               (not (file-directory-p file)))
          (view-(format "message" format-args)ile file)
        ad-do-it))

    ;; unwritable file, don't quit view mode
    (defvar view-mode-force-exit nil)
    (defmacro do-not-exit-view-mode-unless-writable-advice (f)
      `(defadvice ,f (around do-not-exit-view-mode-unless-writable activate)
         (if (and (buffer-file-name)
                  (not view-mode-force-exit)
                  (not (file-writable-p (buffer-file-name))))
             (message "File is unwritable, so stay in view-mode.")
           (progn
             (hl-line-mode 0)
             ad-do-it))))
;;    (do-not-exit-view-mode-unless-writable-advice view-mode-exit)
;;    (do-not-exit-view-mode-unless-writable-advice view-mode-disable)
)
  
  (setcar (cdr (assq 'view-mode minor-mode-alist))
          (if (fboundp 'propertize)
              (list (propertize " View"
                                'face '(:foreground "white"
                                                    :background "DeepPink1")))
            " View"))

  ;; viewmodeのキーバインドを優先
  (add-hook 'view-mode-hook
            '(lambda ()
               (setq minor-mode-Map-Alist
                     (delete (assq 'view-mode minor-mode-map-alist)
                             minor-mode-map-alist)
                     minor-mode-map-alist
                     (cons (cons 'view-mode view-mode-map) minor-mode-map-alist)))))

(use-package autoinsert
  :config
  (setq ;; auto-insert-query nil
   ;; auto-insert-alist nil
   auto-insert-directory "~/.emacs.d/template/")
  (auto-insert-mode 1))

(use-package recentf
  :config
  (defmacro with-suppressed-message (&rest body)
    "Suppress new messages temporarily in the echo area
and the `*Messages*' buffer while BODY is evaluated."
    (declare (indent 0))
    (let ((message-log-max nil))
      `(with-temp-message (or (current-message) "") ,@body)))
  
  (setq recentf-save-file (user-setting-directory ".recentf"))
  (setq recentf-max-saved-items 1000)            ;; recentf に保存するファイルの数
  (setq recentf-exclude '(".recentf"))           ;; .recentf自体は含まない
  (setq recentf-auto-cleanup 'never)             ;; 保存する内容を整理
  (run-with-idle-timer 30 t '(lambda ()          ;; 30秒ごとに .recentf を保存
                               (with-suppressed-message (recentf-save-list))))
  (use-package recentf-ext :ensure t))

(use-package table
  ;; make table
  ;; M-x table-insert to make table with size
  ;; M-x table-capture to make table with csv like text
  ;; M-x org-table-convert to make table from org-mode's table

  ;; M-x table-unrexognize-table to text
  ;; M-x table-rexognize-table to table
  
  ;; TAB to move cell
  ;; C->, C-< to adjust column width
  ;; C-}, C-{ to adjust row hight
  ;; C-- to split cell horizontal
  ;; C-| to split cell vertical
  ;; C-* to merge cell
  ;; C-+ to add row
  ;; C-^ to convert HTML, LaTeX
  ;; C-: to adjust text

  ;; in org mode
  ;; out of org's table, C-c ~ to make table (M-x table-insert)
  ;; in org's table, C-c ~ to convert table (M-x org-table-convert)
  ;; in org's table, C-c ' to edit table
  )

(use-package which-func
  :config
  ;; disp current func name in modeline
  (which-function-mode 1))
(use-package simple
  :config
  ;; show all output of eval
  (setq eval-expression-print-length nil
        ;; C-u C-SPC C-SPC to pop mark
        set-mark-command-repeat-pop  t))
(use-package paren
  :config
  ;; bright parent
  (show-paren-mode t)
  ;; bright region when ending paren not displaying
  (setq show-paren-style 'mixed))
(use-package saveplace
  :config
  (setq-default save-place t)
  (setq save-place-file (user-setting-directory "places")))

(provide '10_standard-elisp)
;;; 10_standard-elisp.el ends here
