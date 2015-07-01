;;; vc-rational-synergy-history.el --- IBM Rational Synergy history
 
;; Copyright (C) 2015 Realworld OO Systems B.V., the Netherlands
;; Copyright (C) 2003 Realworld Systems
 
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
 
;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
 
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.


;; Author: 
;;     Sjoerd van Leent <sjoerd.van.leent@realworld-systems.com>
;;     
;; Maintainer:
;;     Sjoerd van Leent <sjoerd.van.leent@realworld-systems.com>
;;
;; Homepage: http://github.com/RealworldSystems/emacs-rational-synergy
;;
;; Keywords: ibm rational synergy telelogic vc version-control


;;; Commentary:

;; This is a completely redesigned module, as it is not possible
;; anymore to show the graphical user interface through the CLI
;; interface. Therefore, the solution here is a combination of
;; the Graphical display and the way emacs is capable to show
;; semi-graphics.


;;; Code:

;;;; CLI.


(defun vc-rational-synergy--command-history-file (file-name)
  "Attempts to gather the history of a file, for basic information
This is a wrapper around `vc-rational-synergy-command-w/format-to-list'"
  (condition-case err
      (vc-rational-synergy-command-w/format-to-list
       `("history" ,file-name)
       'object-name 'name 'version 'type 'instance 'task 'owner)
    (error nil)))

(defun vc-rational-synergy-command-grab-comments (file-name)
  "Attempts to gather the history of a file, for basic information
This is a wrapper around `vc-rational-synergy-command'"
  (condition-case err
      (let ((raw-comments (vc-rational-synergy-command
			   `("nch" "-f" ">>>%objectname\t%comment")))
	    result current-object-name current-line pivot)
	;; For each line, detect if >>> is present, if so, this is a new
	;; comment. If not, it is an old comment, and the line needs
	;; to be appended without the strip with the first tab.
	;; After >>> immediately the object name follows, separated by a
	;; tab character.
	(dolist (raw raw-comments)
	  (if (and (> (length raw) 3)
		   (string= ">>>" (substring raw 0 3)))
	      (progn
		;; Push current-line and current-object-name into result
		(setq result (cons `(,current-object-name . ,current-line) result))
		;; Extract object name
		(let* ((less-raw (substring raw 4)))
		  (setq pivot (string-match "\\t" less-raw))
		  (setq current-object-name (substring less-raw 0 pivot))
		  (setq current-line (substring less-raw (+ 1 pivot)))))
	    (setq current-line (concat current-line "\n" (substring raw (+ 5 pivot))))))
	;; push last line (if available)
	(when current-line
	  (setq result (cons `(,current-object-name . ,current-line) result)))
	result)
    (error nil)))

(defun vc-rational-synergy--command-get-successors (objectname)
  "Using the OBJECT name find the successors"
  (condition-case err
      (vc-rational-synergy-command-w/format-to-list
       `("query -u" ,(format "has_predecessor('%s')" objectname))
       'objectname)
    (error nil)))

(defun vc-rational-synergy--command-get-predecessors (objectname)
  "Using the OBJECT name find the predecessors"
  (condition-case err
      (vc-rational-synergy-command-w/format-to-list
       `("query -u" ,(format "has_successors('%s')" objectname))
       'objectname)
    (error nil)))
  



(provide 'vc-rational-synergy-history)

;;; vc-rational-synergy-history.el ends here
