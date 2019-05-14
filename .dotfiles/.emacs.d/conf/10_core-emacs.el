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
  :config
  ;; pure-bytes-used => using memory
  :setq ((gc-cons-threshold . (* 512 1024 1024)) ; memmory allocate with 512MB
         (garbage-collection-messages . t)))     ; show message when GC

(leaf atimer.c
  :doc "Asynchronous timers.")

(leaf bidi.c
  :doc "Low-level bidirectional buffer/string-scanning functions.")

(leaf buffer.c
  :doc "Buffer manipulation primitives."
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
  :setq-default ((tab-width      . 8)
                 (fill-column    . 80)
                 (truncate-lines . t)        ; don't wrap lines
                 (cursor-type    . t)        ; use specified for the frame
                 (line-spacing   . 0.0)      ; line-spacing (double is relative)
                 (buffer-file-coding-system . 'utf-8-unix))
  :config
  (scroll-bar-mode 0)
  (put 'erase-buffer 'disabled nil))

(leaf bytecode.c
  :doc "Execution of byte code produced by bytecomp.el.")

(leaf callint.c
  :doc "Call a Lisp function interactively."
  ;; prefix-arg ?
  ;; command-history (a list of command-history)
  ;; command-debug-status (debugging status of current interactive command)
  ;; mark-even-if-inactive ?
  ;; mouse-leave-buffer-hook ?
  )

(leaf callproc.c
  :doc "Synchronous subprocess invocation."
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
  :setq ((shell-file-name . "/bin/bash")))        ; path of default shell

(leaf casefiddle.c
  :doc "Case conversion functions.")

(leaf casetab.c
  :doc "Routines to deal with case tables.")

(leaf category.c
  :doc "Routines to deal with category tables.")

(leaf ccl.c
  :doc "CCL (Code Conversion Language) interpreter.")

(leaf character.c
  :doc "Basic character support.")

(leaf charset.c
  :doc "Basic character set support.")

(leaf chartab.c
  :doc "char-table support.")

(leaf cm.c
  :doc "Cursor motion subroutines.")

(leaf cmds.c
  :doc "Simple built-in editing commands."
  ;; post-self-insert-hook
  )

(leaf coding.c
  :doc "Coding system handler (conversion, detection, etc)."
  ;; coding-system-list (detectable coding-systems)
  ;; coding-system-alist (alist ver, codiing-system-list)
  ;; coding-category-list (List of coding-categories  ordered by priority)
  ;; coding-system-for-read
  ;; coding-system-for-write
  :setq ((locale-coding-system . 'utf-8-unix) ; system I/O, decode keyboard input
         ;; coding systems used for process I/O by default.
         (default-process-coding-system . '(utf-8-unix . utf-8-unix))))

(leaf sygw32.c
  :doc "Cygwin support routines.")

(leaf data.c
  :doc "Primitive operations on Lisp data types for Lisp interpreter.")

(leaf dbusbind.c
  :doc "Elisp bindings for D-Bus. (interprocess communication)")

(leaf decompress.c
  :doc "Interface to zlib.")

(leaf dired.c
  :doc "Lisp functions for making directory listings."
  ;; completion-ignored-extensions (not to competion file end one of.)
  ;;  => (".o" "~" ".bin" ".lbin" ".so" ".a" ".ln" ".blg" ".bbl" ".elc" ...)
  )

(leaf dispnew.c
  :doc "Updating of data structures for redisplay."
  ;; initial-window-system (window system for the first frame)
  ;; window-system (window system for current frame (x, w32, mac, ns, pc))
  :setq ((visible-bell . nil)))         ; non-nil enable visible-bell


(leaf doc.c
  :doc "Record indices of function doc strings stored in a file.")

(leaf doprint.c
  :doc "Output like sprintf to a buffer of specified size.")

(leaf dosfns.c
  :doc "MS-DOS specific Lisp utilities.")

(leaf dynlib.c
  :doc "Portable API for dynamic loading.")

(leaf indent.c
  :doc "Indentation functions"
  :setq-default ((indent-tabs-mode . nil)))

(leaf minibuf.c
  :doc ""
  :setq ((enable-recursive-minibuffers . t)))

(leaf fontset.c
  :doc "commands for handling fontset"
  :when window-system
  :doc "
|--------------+---------------------------|
| digit        | 01234, 56789,             |
| abc          | abcdef ghijkl             |
| ABC          | ABCDEF GHIJKL             |
| ascii 20~    | !#$%&' ()*+,-             |
|              | ∩∪∞≤≥∏ ∑∫×±⊆⊇    |
|              | αβγδεζ ηθικλμ |
|              | ΑΒΓΔΕΖ ΗΘΙΚΛΜ |
| kanji        | 日本語 の美観             |
| kana         | あいう えおか             |
| katakana     | アイウ エオカ             |
| han-katakana | ｱｲｳｴｵｶ ｷｸｹｺｻｼ             |
|--------------+---------------------------|
"
  :doc "
$ tree -L 1
.
├── Makefile
├── Makefunc.mk
├── auto-save-list
├── conf
├── init.el
├── latex-math-preview-cache
├── local
├── site-lisp
├── snippets
└── templete

7 directories, 5 files
"
  :config
  (cond
   ((and (member "Hack Nerd Font" (font-family-list))
         (member "Hiragino Kaku Gothic Pro" (font-family-list)))
    (leaf *hack-nerd-font
      :doc "https://github.com/ryanoasis/nerd-fonts#option-4-homebrew-fonts"
      :doc "http://extra-vision.blogspot.com/2016/07/emacs.html"
      :doc "
brew tap caskroom/fonts
brew cask install font-hack-nerd-font"
      :init
      (create-fontset-from-ascii-font "Hack Nerd Font" nil "hack_nerd")
      (set-fontset-font
       "fontset-hack_nerd" 'unicode
       (font-spec :family "Hiragino Kaku Gothic Pro" :size 14) nil 'append)
      :custom ((default-frame-alist . '((font . "fontset-hack_nerd"))))))))

(leaf macterm.c
  :doc "Implementation of GUI terminal on macOS"
  :doc "Each SYMBOL can be `control', `meta', `alt', `hyper', or `super'"
  :doc "`left' meens same value setting its left key"
  :when (eq window-system 'mac)
  :custom ((mac-control-modifier       . 'control)
           (mac-option-modifier        . 'super)
           (mac-command-modifier       . 'meta)

           (mac-right-control-modifier . 'control)
           (mac-right-option-modifier  . 'hyper)
           (mac-right-command-modifier . 'meta)

           ;; use fn key as normal way.
           ;; (mac-function-modifier      . 'super)
           ))

(leaf casefiddle.c
  :doc "GNU Emacs case conversion functions"
  :config
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil))

(provide '01_core-emacs)
;;; init.el ends here
