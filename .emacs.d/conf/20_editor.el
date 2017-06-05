;;; 20_editor.el ---                                 -*- lexical-binding: t; -*-

;; Copyright (C) 2017  Naoya Yamashita

;; Author: Conao
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
(use-package helm :ensure t :diminish ""
  :bind (("M-x"     . helm-M-x)
         ("C-x b"   . helm-mini)
         ("C-x C-f" . helm-find-files)
         ("C-x C-r" . helm-recentf)
         ("C-x b"   . helm-buffers-list)
         ("C-x g"   . helm-google-suggest)
         ("C-R"     . helm-regexp)
         ("M-O"     . helm-occur)
         ("M-y"     . helm-show-kill-ring)
         ("C-c h"   . helm-command-prefix)
         :map helm-command-map
         ("o"       . helm-occur)
         :map helm-map
         ("<tab>"   . helm-execute-persistent-action)
         ("C-i"     . helm-execute-persistent-action)
         ("C-z"     . helm-select-action))
  :config
  (use-package helm-config)
  
  ;; Change helm-command-prefix "C-x c" to "c-c h"
  ;; default "C-x c" is quite close to "C-x C-c" which quits Emacs
  ;;  (global-set-key (kbd "C-c h") helm-command-map)
  (global-unset-key (kbd "C-x c"))
  
  (when (executable-find "curl")
    (setq helm-google-suggest-use-curl-p t))
  
  (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
        helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
        helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
        helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
        helm-ff-file-name-history-use-recentf t
        helm-echo-input-in-header-line nil)
  
  ;; helm window setting
  (setq helm-autoresize-max-height 0)
  (setq helm-autoresize-min-height 40)
  (helm-autoresize-mode t)
  
  ;; enable fuzzy seach in helm-mini, semantic
  (setq helm-M-x-fuzzy-match        t
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match    t
        helm-semantic-fuzzy-match   t
        helm-imenu-fuzzy-match      t
        helm-apropos-fuzzy-match    t)
  
  (setq helm-idle-delay 0.0)	; 候補を作って描写するまでのタイムラグ。デフォルトで 0.3
  (setq helm-input-idle-delay 0.0) ; 文字列を入力してから検索するまでのタイムラグ。デフォルトで 0
  (setq helm-candidate-number-limit 100) ; 表示する最大候補数。デフォルトで 50

  ;; use man in helm (C-c h m)
  (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
  
  (helm-mode 1)

  ;; "M-a" to select(mark) all
  ;; "C-c C-i" to insert file path to current buffer

  ;; "C-w" to yank words after cursor
  ;; "M-n" to yank symbol at cursor

  ;; "TAB" to disp command help in "Helm-M-x" buffer

  ;; helm-mini
  ;; pattern "*<major-mode>" to match that major-mode
  ;; pattern "*!<major-made>" to not match that major-mode
  ;; pattern "/directory" to match that directory
  ;; pattern "!/directory" to not match that directory
  ;; pattern "@hoge" to search hoge in all buffer
  ;; then type "C-s" to disp line num (switch helm-moccur)

  ;; helm-find-files
  ;; "C-l" to find above directory
  ;; "C-r" to find previous directory
  ;; "C-s" to do grep at current directory
  ;; "C-u C-s" to do recursive grep at current directory
  ;; add "~/" to home directory
  ;; add "/"  to root directory
  ;; add "./" to current directory
  ;; "C-c h" to disp file history
  
  ;; split-root
  ;;   (require 'split-root)
  ;;   (setq helm-compilation-window-height-percent 20.0)
  ;;   (defun helm-compilation-window-root (buf)
  ;;     (setq helm-compilation-window
  ;;           (split-root-window (truncate (* (window-height)
  ;;                                           (/ helm-compilation-window-height-percent
  ;;                                              100.0)))))
  ;;      (set-window-buffer helm-compilation-window buf))
  
  ;;   (setq helm-display-function 'helm-compilation-window-root)
  )

(use-package flex-autopair :ensure t :diminish ""
  :config
  (setq flex-autopair 1))

(use-package auto-complete :ensure t :diminish ""
  :bind (:map ac-menu-map
              ("C-n" . ac-next)
              ("C-p" . ac-previous))
  :init
  (use-package fuzzy :ensure t)
  (use-package pos-tip :ensure t)
  
  :config
  (use-package auto-complete-config)
  (ac-config-default)
  (setq ac-auto-show-menu   0
        ac-delay            0
        ac-quick-help-delay 1
        ac-menu-height      15
        ac-auto-start       1
        ac-use-menu-map     t)
  (push 'ac-source-filename ac-sources)
  
  (ac-flyspell-workaround)
  (add-to-list 'ac-modes 'text-mode)
  (add-to-list 'ac-modes 'fundamental-mode)
  (add-to-list 'ac-modes 'org-mode)
  (add-to-list 'ac-modes 'yatex-mode)
  
  (push 'ac-source-filename ac-sources))

(use-package yascroll :ensure t
  :config
  (global-yascroll-bar-mode 1))

(use-package undohist :ensure t
  :config
  (undohist-initialize)
  (setq undohist-directory (user-setting-directory "undohist")
        undohist-ignored-files '("/tmp" "elpa" "el-get")))

(use-package undo-tree :ensure t :diminish ""
  :config
  (global-undo-tree-mode)
  ;; Quick-Start
  ;; ===========
  ;; `undo-tree-mode' and `global-undo-tree-mode'
  ;;   Enable undo-tree mode (either in the current buffer or globally).
  ;;
  ;; C-_  C-/  (`undo-tree-undo')
  ;;   Undo changes.
  ;;
  ;; M-_  C-?  (`undo-tree-redo')
  ;;   Redo changes.
  ;;
  ;; `undo-tree-switch-branch'
  ;;   Switch undo-tree branch.
  ;;   (What does this mean? Better press the button and see!)
  ;;
  ;; C-x u  (`undo-tree-visualize')
  ;;   Visualize the undo tree.
  ;;   (Better try pressing this button too!)
  ;;
  ;; In the undo-tree visualizer:
  ;;
  ;; <up>  p  C-p  (`undo-tree-visualize-undo')
  ;;   Undo changes.
  ;;
  ;; <down>  n  C-n  (`undo-tree-visualize-redo')
  ;;   Redo changes.
  ;;
  ;; <left>  b  C-b  (`undo-tree-visualize-switch-branch-left')
  ;;   Switch to previous undo-tree branch.
  ;;
  ;; <right>  f  C-f  (`undo-tree-visualize-switch-branch-right')
  ;;   Switch to next undo-tree branch.
  ;;
  ;; C-<up>  M-{  (`undo-tree-visualize-undo-to-x')
  ;;   Undo changes up to last branch point.
  ;;
  ;; C-<down>  M-}  (`undo-tree-visualize-redo-to-x')
  ;;   Redo changes down to next branch point.
  ;;
  ;; <down>  n  C-n  (`undo-tree-visualize-redo')
  ;;   Redo changes.
  ;;
  ;; <mouse-1>  (`undo-tree-visualizer-mouse-set')
  ;;   Set state to node at mouse click.
  ;;
  ;; t  (`undo-tree-visualizer-toggle-timestamps')
  ;;   Toggle display of time-stamps.
  ;;
  ;; d  (`undo-tree-visualizer-toggle-diff')
  ;;   Toggle diff display.
  ;;
  ;; s  (`undo-tree-visualizer-selection-mode')
  ;;   Toggle keyboard selection mode.
  ;;
  ;; q  (`undo-tree-visualizer-quit')
  ;;   Quit undo-tree-visualizer.
  ;;
  ;; C-q  (`undo-tree-visualizer-abort')
  ;;   Abort undo-tree-visualizer.
  ;;
  ;; ,  <
  ;;   Scroll left.
  ;;
  ;; .  >
  ;;   Scroll right.
  ;;
  ;; <pgup>  M-v
  ;;   Scroll up.
  ;;
  ;; <pgdown>  C-v
  ;;   Scroll down.
  ;;
  ;; In visualizer selection mode:
  ;;
  ;; <up>  p  C-p  (`undo-tree-visualizer-select-previous')
  ;;   Select previous node.
  ;;
  ;; <down>  n  C-n  (`undo-tree-visualizer-select-next')
  ;;   Select next node.
  ;;
  ;; <left>  b  C-b  (`undo-tree-visualizer-select-left')
  ;;   Select left sibling node.
  ;;
  ;; <right>  f  C-f  (`undo-tree-visualizer-select-right')
  ;;   Select right sibling node.
  ;;
  ;; <pgup>  M-v
  ;;   Select node 10 above.
  ;;
  ;; <pgdown>  C-v
  ;;   Select node 10 below.
  ;;
  ;; <enter>  (`undo-tree-visualizer-set')
  ;;   Set state to selected node and exit selection mode.
  ;;
  ;; s  (`undo-tree-visualizer-mode')
  ;;   Exit selection mode.
  ;;
  ;; t  (`undo-tree-visualizer-toggle-timestamps')
  ;;   Toggle display of time-stamps.
  ;;
  ;; d  (`undo-tree-visualizer-toggle-diff')
  ;;   Toggle diff display.
  ;;
  ;; q  (`undo-tree-visualizer-quit')
  ;;   Quit undo-tree-visualizer.
  ;;
  ;; C-q  (`undo-tree-visualizer-abort')
  ;;   Abort undo-tree-visualizer.
  ;;
  ;; ,  <
  ;;   Scroll left.
  ;;
  ;; .  >
  ;;   Scroll right.
  )

(use-package flycheck :ensure t
  :config
  (use-package flycheck-pos-tip :ensure t)
  
  (global-flycheck-mode)
  (custom-set-variables
   '(flycheck-keymap-prefix           (kbd "C-c f"))
   '(flycheck-display-errors-function #'flycheck-pos-tip-error-messages)))
  

(provide '20_editor)
;;; 20_editor.el ends here
