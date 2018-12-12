;;; 01_core-emacs.el ---                             -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Naoya Yamashita

;; Author: Naoya Yamashita
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

;; Storage allocation and gc for GNU Emacs Lisp interpreter.
(leaf alloc.c
  :require nil
  :config
  ;; pure-bytes-used => using memory
  (setq gc-cons-threshold (* 128 1024 1024) ; memmory allocate with 128MB
        garbage-collection-messages t       ; show message when GC
        ))

;; Asynchronous timers.
(leaf atimer.c
  :require nil)

;; Low-level bidirectional buffer/string-scanning functions.
(leaf bidi.c
  :require nil)

;; Buffer manipulation primitives.
(leaf buffer.c
  :require nil
  ;; header-line-format (buffer header line format, buffer local)
  ;; mode-line-format
  ;; major-mode => lisp-interaction-mode
  ;; mode-name => "Lisp Interaction"
  ;; ctl-arrow ?
  ;; ward-wrap (non-nil meens don't chop word when wrap line)
  ;; default-directory (default path for each buffer)
  ;; auto-fill-function ?
  ;; buffer-file-name (filename for each buffer)
  ;; buffer-read-only (non-nil meens read-only buffer)
  ;; buffer-saved-size ?
  ;; selective-display ?
  ;; overwrite-mode ('overwrite-mode-textual to overwrite or nil)
  ;; buffer-display-table ?
  ;; left-margin-width ?
  ;; left-fringe-width ?
  ;; fringes-outside-margins ?
  ;; scroll-bar-width ?
  ;; indicate-empty-lines (non-nil to display file end indicator on left fringe)
  ;; indicate-buffer-boundaries (non-nil to display file end marker on fringe)
  ;; fringe-indicator-alist, fringe-cursor-alist
  ;; mark-active (non-nil active mark)
  (setq-default tab-width      8
                fill-column    80
                truncate-lines t        ; don't wrap lines
                cursor-type    t        ; use specified for the frame
                line-spacing   0.0      ; line-spacing (double is relative)
                buffer-file-coding-system 'utf-8-unix
                )
  
(leaf indent.c
  :require nil
  :config
  (setq-default indent-tabs-mode nil))

(provide '01_core-emacs)
;;; init.el ends here
