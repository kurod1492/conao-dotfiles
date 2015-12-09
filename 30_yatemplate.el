;;; 22_yatemplate.el ---

;; Copyright (C) 2015 Naoya Yamashita
;; Author: Naoya Yamashita
;; Created:      <2015/11/19 13:20:08>
;; Last-Updated: <2015/11/30 17:15:29>
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

;; http://d.hatena.ne.jp/yutoichinohe/20130607/1370627890

(require 'autoinsert)
(auto-insert-mode 1)
(setq auto-insert-query nil)

(setq auto-insert-alist nil)
(setq auto-insert-alist
      (append '(
 		(("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
 		 (upcase (concat (file-name-nondirectory
 				  (file-name-sans-extension buffer-file-name))
 				 "_"
 				 (file-name-extension buffer-file-name)))
 		 "#ifndef " str \n
 		 "#define " str "\n\n"
 		 _ "\n\n#endif")
 		) auto-insert-alist))

(setq auto-insert-alist
      (append '(
 		(("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
 		 nil
 		 "#include \""
 		 (let ((stem (file-name-sans-extension buffer-file-name)))
 		   (cond ((file-exists-p (concat stem ".h"))
 			  (file-name-nondirectory (concat stem ".h")))
 			 ((file-exists-p (concat stem ".hh"))
 			  (file-name-nondirectory (concat stem ".hh")))))
 		 & ?\" | -10)
 		) auto-insert-alist))

(setq auto-insert-alist
      (append '(
		(html-mode . (lambda () (sgml-tag "html")))
		) auto-insert-alist))

(setq auto-insert-alist
      (append '(
		(latex-mode
		 ;; should try to offer completing read for these
		 "options, RET: "
		 "\\documentclass[" str & ?\] | -1
		 ?{ (read-string "class: ") "}\n"
		 ("package, %s: "
		  "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
		 _ "\n\\begin{document}\n" _
		 "\n\\end{document}")
 		) auto-insert-alist))

(setq auto-insert-alist
      (append '(
		(("\\.el\\'" . "Emacs Lisp header")
		 "Short description: "
		 ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str
		 "

;; Copyright (C) " (format-time-string "%Y") " "
					      (getenv "ORGANIZATION") | (progn user-full-name) "
;; Author: " (user-full-name)
	      '(if (search-backward "&" (line-beginning-position) t)
		   (replace-match (capitalize (user-login-name)) t t))
	      '(end-of-line 1) "
;; Keywords: "
	      '(require 'finder)
	      ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
	      '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
				finder-known-keywords)
		     v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
				   finder-known-keywords
				   "\n"))
	      ((let ((minibuffer-help-form v2))
		 (completing-read "Keyword, C-h: " v1 nil t))
	       str ", ") & -2 "
;; Created:      " (format-time-string "<%Y/%m/%d %H:%M:%S>" (current-time)) "
;; Last-Updated: <>

\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

\;;; Commentary:

\;; 

\;;; Code:
" _ ;; "
 ;; \(provide '"
 ;; (file-name-base)
 ;; ")
 ;; \;;; " (file-name-nondir
 ;;	 ectory (buffer-file-name)) " ends here\n"
 )
 		) auto-insert-alist))
