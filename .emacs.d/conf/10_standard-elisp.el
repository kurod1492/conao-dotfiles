;;; 10_standard-elisp.el

;;; Code:

(use-package generic-x
  :config
  (global-font-lock-mode))

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
  ;; :defer t
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
  ;; :defer t
  :config
  (global-linum-mode t)
  (setq linum-delay nil
        linum-format "%5d")
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

(use-package wdired
  :config
  (setq delete-by-moving-to-trash t)
  (bind-key "r" 'wdired-change-to-wdired-mode dired-mode-map)

  ;; o . open dired
  )

(use-package view
  :config
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

(use-package outline
  :config
  (bind-key "<tab>" 'org-cycle outline-minor-mode-map)
  (bind-key "C-<tab>" 'org-global-cycle outline-minor-mode-map)
  (bind-key "C-c C-f" 'outline-forward-same-level outline-minor-mode-map)
  (bind-key "C-c C-b" 'outline-backward-same-level outline-minor-mode-map)
  (bind-key "C-c C-n" 'outline-next-visible-heading outline-minor-mode-map)
  (bind-key "C-c C-p" 'outline-previous-visible-heading outline-minor-mode-map)
  (bind-key "<tab>" 'org-cycle outline-mode-map)
  (bind-key "S-<tab>" 'org-global-cycle outline-mode-map))
