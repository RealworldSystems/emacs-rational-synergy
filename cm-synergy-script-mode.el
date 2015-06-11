;;; cm-synergy-script-mode.el --- mode for editing scripting files for CM Synergy
 
;; Copyright (C) 2004  Realworld Systems
 
;; Author: GR <support@realworldwide.com>
;; Created: Jun/2004
;; Updated: Jun/2004
;; Keywords: lisp  
 
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 
;;; Commentary:
 
;; 
 
;;; Code:

(define-derived-mode cm-synergy-script-mode text-mode "CMSynergyScript"
  "Realworld major-mode derived from text-mode for editing script files for CM Synergy Command Line, files should have extension .syn"
  (set (make-local-variable 'comment-start) "#")
  (set (make-local-variable 'comment-start-skip) "\\(\\(^\\|[^\\\\\n]\\)\\(\\\\\\\\\\)*\\)\\(\\s<+\\|#+\\)[ \t]*")
  (set (make-local-variable 'font-lock-defaults) '(cm-synergy-script-mode-font-lock-keywords t))
  (font-lock-mode 1)
  )

(defconst cm-synergy-script-mode-font-lock-keywords
   '(
     ("#.*$" .				     font-lock-comment-face)	   ;; comments
     ("^\\([^ \t\n]+\\)[ \t\n]"		     (1 font-lock-function-name-face))   ;; commands
     ("[ \t]+\\([\-/][^ \t]+\\)[ \t]"	     (1 font-lock-reference-face)) ;; parameters
     ("[^\"]\"\\([^\"]+\\)\""		     (1 font-lock-string-face))	   ;; strings
     )
   "*    Date          : Dec/2000
  Author        : Realworld OO Systems B.V. (GR)."
   )

;; ----------
;; add regexp to auto-mode-alist so the mode gets set automatically
;; ----------
(or (assoc "\.syn$" auto-mode-alist)
    (push '("\.syn$" . cm-synergy-script-mode) auto-mode-alist))

(provide 'cm-synergy-script-mode)

;;; cm-synergy-script-mode.el ends here
