;;; vc-rational-synergy-tabular.el --- IBM Rational Synergy tabular formatting
 
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

;; Used to be able to format items in a table, such that it can be
;; displayed in a buffer.

;;; Code:

(defun vc-rational-synergy--tabular-task-files (task-files &optional wa-root)
  "The task files are made to a string with a heading and the task files.
This method does some formatting calculation to be able to render the
task files in a tabular format into the vc-rational-synergy buffer."
  (let* ((file-name-heading vc-rational-synergy-int-file-name)
	 (type-heading vc-rational-synergy-int-type)
	 (rev-heading vc-rational-synergy-int-rev)
	 (instance-heading vc-rational-synergy-int-instance)
	 (status-heading vc-rational-synergy-int-status)
	 (length-of-file-name (length file-name-heading))
	 (length-of-type (length type-heading))
	 (length-of-rev (length rev-heading))
	 (length-of-instance (length instance-heading))
	 (length-of-status (length status-heading))
	 (length-of-wa-root (if wa-root (length wa-root) 0)))
   
    ;; Stretch the columns if necessary

    (dolist (task-file task-files)
      (let ((fnl (length (vc-rational-synergy-task-file-name task-file)))
	    (tl (length (vc-rational-synergy-task-file-type task-file))))
	(when (> fnl length-of-file-name) (setq length-of-file-name fnl))
	(when (> tl length-of-type) (setq length-of-type tl))))
    
    (let* (;; The meta format is based on the length of either the headings
	   ;; or the contents, this is precalculated for length-of-file-name
	   ;; and for length-of-rev
	   (meta-format (format "%%-%ds %%-%ds %%%ds %%%ds %%-%ds"
				length-of-file-name
				length-of-type
				length-of-rev
				length-of-instance
				length-of-status))
	   ;; The heading is the first line to be displayed, rendered
	   ;; properly against the meta format
	   (heading (format meta-format 
			    file-name-heading type-heading 
			    rev-heading instance-heading
			    status-heading))
	   ;; The line is the separator of the actual values versus the
	   ;; heading
	   (line (format meta-format
			 (make-string length-of-file-name ?-)
			 (make-string length-of-type ?-)
			 (make-string length-of-rev ?-)
			 (make-string length-of-instance ?-)
			 (make-string length-of-status ?-)))
	   ;; The result will eventually contain the listing of the values
	   ;; prefixed by the heading and separator line
	   
	   (wa-root-string (if wa-root
			       (format ">>> Work Area: %s <<<\n\n" wa-root)
			     ""))

	   (result (concat wa-root-string heading "\n" line "\n")))

      ;; Loops over each individual task file and retrieves the name,
      ;; type, revision and instance information and adds them to result
      
      (dolist (tf task-files)
	(setq result
	      (concat result 
		      (format meta-format
			      (vc-rational-synergy-task-file-name tf)
			      (vc-rational-synergy-task-file-type tf)
			      (vc-rational-synergy-task-file-revision tf)
			      (vc-rational-synergy-task-file-instance tf)
			      (vc-rational-synergy-task-file-status tf))))
	(condition-case err
	    (setq result
		  (concat result
			  (format " ---> %s"
				  (substring 
				   (vc-rational-synergy-task-file-path tf)
				   (+ 1 length-of-wa-root)))))
	  (no-method-definition nil))
	(setq result (concat result "\n")))
	
      result)))

(defun vc-rational-synergy--tabular-sf (simple-files what)
  "Tabulates a simple-files list"
  (let* ((meta-format (format "%%-%ds %%-%ds" 15 64))
	 (heading (format meta-format "Type" "Name"))
	 (line (format meta-format
		       (make-string 15 ?-)
		       (make-string 64 ?-)))
	 (real-list (sort simple-files 
			  (lambda (a b)
			    (if (eq (car a) (car b))
				(string< (cdr a) (cdr b))
			      (string< (symbol-name (car a)) 
				       (symbol-name (car b)))))))
	 (result (concat what "\n\n" heading "\n" line "\n")))
    (dolist (sf real-list)
      (setq result
	    (concat result "\n"
		    (format meta-format
			    (if (eq (car sf) 'dir) "Directory" "File")
			    (cdr sf)))))
    result))


(provide 'vc-rational-synergy-tabular)

;; vc-rational-synergy-tabular.el ends here
