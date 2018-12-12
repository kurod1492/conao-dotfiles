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

(leaf alloc.c
  :doc "Storage allocation and gc for GNU Emacs Lisp interpreter."
  :require nil
  :config
  ;; pure-bytes-used => using memory
  (setq gc-cons-threshold (* 128 1024 1024) ; memmory allocate with 128MB
        garbage-collection-messages t       ; show message when GC
        ))

(leaf atimer.c
  :doc "Asynchronous timers."
  :require nil)

(leaf bidi.c
  :doc "Low-level bidirectional buffer/string-scanning functions."
  :require nil)

(leaf buffer.c
  :doc "Buffer manipulation primitives."
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
                ))

(leaf bytecode.c
  :doc "Execution of byte code produced by bytecomp.el."
  :require nil)

(leaf callint.c
  :doc "Call a Lisp function interactively."
  :require nil
  ;; prefix-arg ?
  ;; command-history (a list of command-history)
  ;; command-debug-status (debugging status of current interactive command)
  ;; mark-even-if-inactive ?
  ;; mouse-leave-buffer-hook ?
  )

(leaf callproc.c
  :doc "Synchronous subprocess invocation."
  :require nil
  ;; exec-path (in environment variable, $PATH)
  ;; exec-suffixes (maybe use "g" to find ggrep, gfind"
  ;; exec-directory
  ;;  => "/Applications/Emacs-26.1.app/Contents/MacOS/libexec/"
  ;; doc-directory
  ;;  => "/Applications/Emacs-26.1.app/Contents/Resources/etc/"
  ;; configure-info-directory
  ;;  => "/Users/conao/Documents/emacs-26.1/emacs-mac-build/share/info"
  ;; initial-environment (environment variable when Emacs build)
  ;; process-environment ?
  (setq shell-file-name "/bin/bash"     ; path of default shell
        ))

(leaf casefiddle.c
  :doc "Case conversion functions."
  :require nil)

(leaf casetab.c
  :doc "Routines to deal with case tables."
  :require nil)

(leaf category.c
  :doc "Routines to deal with category tables."
  :require nil)

(leaf ccl.c
  :doc "CCL (Code Conversion Language) interpreter."
  :require nil)

(leaf character.c
  :doc "Basic character support."
  :require nil)

(leaf charset.c
  :doc "Basic character set support."
  :require nil)

(leaf chartab.c
  :doc "char-table support."
  :require nil)

(leaf cm.c
  :doc "Cursor motion subroutines."
  :require nil)

(leaf cmds.c
  :doc "Simple built-in editing commands."
  :require nil
  ;; post-self-insert-hook
  )

(leaf coding.c
  :doc "Coding system handler (conversion, detection, etc)."
  :require nil
  ;; coding-system-list (detectable coding-systems)
  ;; coding-system-alist (alist ver, codiing-system-list)
  ;; coding-category-list (List of coding-categories  ordered by priority)
  ;; coding-system-for-read
  ;; coding-system-for-write
  (setq locale-coding-system 'utf-8-unix ; system I/O, decode keyboard input
        ;; coding systems used for process I/O by default.
        default-process-coding-system '(utf-8-unix . utf-8-unix)))

(leaf sygw32.c
  :doc "Cygwin support routines."
  :require nil)

(leaf data.c
  :doc "Primitive operations on Lisp data types for Lisp interpreter."
  :require nil)

(leaf dbusbind.c
  :doc "Elisp bindings for D-Bus. (interprocess communication)"
  :require nil)

(leaf indent.c
  :require nil
  :config
  (setq-default indent-tabs-mode nil))

(provide '01_core-emacs)
;;; init.el ends here
