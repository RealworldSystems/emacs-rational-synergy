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
;;
;; Currently only does it's job on file level. also incorporates
;; Ediff functionality.


;;; Code:

(require 'eieio)
(require 'ediff)
(require 'button)
(require 'vc-rational-synergy-authentication)
(require 'vc-rational-synergy-command-to-string)

;;;; CLI.

(defun vc-rational-synergy--command-history-cat (object)
  "Attempts to dump the contents of a particular object
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (condition-case err
      (vc-rational-synergy-command-to-string
       `("cat" ,object))
    (error nil)))

(defun vc-rational-synergy--command-history-task-info (task)
  "Attempts to gather additional task info, for basic information
This is a wrapper around `vc-rational-synergy-command-w/format-to-list'"
  (condition-case err
      (car (vc-rational-synergy-command-w/format-to-list
	    `("task" "-show" "i" ,task)
	    'status 'release))
    (error nil)))


(defun vc-rational-synergy--command-history-file (file-name)
  "Attempts to gather the history of a file, for basic information
This is a wrapper around `vc-rational-synergy-command-w/format-to-list'"
  (condition-case err
      (vc-rational-synergy-command-w/format-to-list
       `("history" ,file-name)
       'objectname 'name 'version 'type 'instance 'task 'owner 'status 'release)
    (error nil)))

(defun vc-rational-synergy--command-grab-comments (file-name)
  "Attempts to gather the history of a file, for basic information
This is a wrapper around `vc-rational-synergy-command'"
  (condition-case err
      (let ((raw-comments (vc-rational-synergy-command
			   `("history" ,file-name
			     "-nch" "-f" ">>>%objectname\t%comment")))
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
		(when (or current-object-name current-line)
		  (setq result (cons `(,current-object-name . ,current-line) result)))
		;; Extract object name
		(let* ((less-raw (substring raw 3))
		       (splitted (split-string less-raw "\t")))
		  (setq pivot (length (car splitted)))
		  (setq current-object-name (car splitted))
		  (setq current-line (mapconcat (function (lambda (x) x)) (cdr splitted) "\t"))))
	    (setq current-line (concat current-line "\n" (substring raw (+ 5 pivot))))))
	;; push last line (if available)
	(when current-line
	  (setq result (cons `(,current-object-name . ,current-line) result)))
	result)
    (error nil)))

(defun vc-rational-synergy--command-get-successors (objectname)
  "Using the OBJECT name find the successors"
  (condition-case err
      (mapcar 'car
	      (vc-rational-synergy-command-w/format-to-list
	       `("query" "-u" ,(format "has_predecessor('%s')" objectname))
	       'objectname))
    (error nil)))

(defun vc-rational-synergy--command-get-predecessors (objectname)
  "Using the OBJECT name find the predecessors"
  (condition-case err
      (mapcar 'car
	      (vc-rational-synergy-command-w/format-to-list
	       `("query" "-u" ,(format "has_successor('%s')" objectname))
	       'objectname))
    (error nil)))


;;;; Linking

(defclass vc-rational-synergy--history-object ()
  ((comment :initarg :comment :reader vc-rational-synergy--hio-comment)
   (objectname :initarg :name :reader vc-rational-synergy--hio-name)
   (properties :initarg :props :reader vc-rational-synergy--hio-props))
  "Object representing an instance of an object in history")


(defun vc-rational-synergy--history-inject-comments (simple-history file-name)
  "Sets up the history comments and inject them into simple-history"
  (let ((comments (vc-rational-synergy--command-grab-comments file-name)))
    (when comments
      (mapcar (lambda (she)
		(cons
		 (car (delq 'nil (mapcar (lambda (com) 
					   (when (equal (car she) (car com))
					     (cdr com)))
					 comments))) she)) simple-history))))

(defun vc-rational-synergy--history-make-objects (c-history)
  "Creates objects out of raw commented-history lists"
  (mapcar (lambda (c)
	    (make-instance 'vc-rational-synergy--history-object
			   :comment (car c)
			   :name (nth 1 c)
			   :props `((name     . ,(nth 2 c))
				    (version  . ,(nth 3 c))
				    (type     . ,(nth 4 c))
				    (instance . ,(nth 5 c))
				    (task     . ,(nth 6 c))
				    (owner    . ,(nth 7 c))
				    (status   . ,(nth 8 c))
				    (release  . ,(nth 9 c)))))
	  c-history))

(defun vc-rational-synergy--history-find-object-by-name (name objects)
  "Finds an object by object name"
  (let (result)
    (dolist (o objects result)
      (when (equal name (vc-rational-synergy--hio-name o))
	(setq result o)))))

(defun vc-rational-synergy--inject-in-pool (list pool)
  "For each element, check availability, if not found cons to pool"
  (dolist (new list pool)
    (let ((found (delq 'nil
		       (mapcar (lambda (p)
				 (when (and (equal (car p) (car new))
					    (equal (cdr p) (cdr new))) t))
			       pool))))
      (unless found
	(setq pool (cons new pool))))))
	    

(defun vc-rational-synergy--history-adjust-pool (object objects predecessors successors pool)
  "inserts elements into the pool of links, if not available already"
  (let ((first-set (mapcar (lambda (predecessor)
			     `(,(vc-rational-synergy--history-find-object-by-name predecessor objects)
			       .
			       ,object))
			   predecessors))
	(second-set (mapcar (lambda (successor)
			     `(,object
			       .
			       ,(vc-rational-synergy--history-find-object-by-name successor objects)))
			    successors)))
    (vc-rational-synergy--inject-in-pool second-set
					 (vc-rational-synergy--inject-in-pool first-set pool))))
			      

(defun vc-rational-synergy--history-setup-links (objects)
  "Given an object, attempts to find predecessors/successors
Associates predecessors and successors in unidirectional links, creating a pool
of links"
  (let ((pool))
    (mapcar (lambda (object)
	      (let* ((name (vc-rational-synergy--hio-name object))
		     (predecessors (vc-rational-synergy--command-get-predecessors name))
		     (successors (vc-rational-synergy--command-get-successors name)))
		(setq pool (vc-rational-synergy--history-adjust-pool object
								     objects
								     predecessors
								     successors
								     pool))))
	    objects)
    pool))
		

(defun vc-rational-synergy-setup-history-for-file (file-name)
  "Sets up the history for a file in terms of objects and links"
  (with-vc-rational-synergy
   (let ((simple-history (vc-rational-synergy--command-history-file file-name)))
     (if simple-history
       (progn
	 (let* ((c-history (vc-rational-synergy--history-inject-comments
			    simple-history file-name))
		(objects (vc-rational-synergy--history-make-objects c-history))
		(links (vc-rational-synergy--history-setup-links objects)))
	   links))
       (vc-rational-synergy-message "No history found for %s" file-name)))))


(defun vc-rational-synergy-setup-history-for-buffer ()
  "Does `vc-rational-synergy-setup-history-for-file' for current buffer file"
  (vc-rational-synergy-setup-history-for-file (buffer-file-name)))

;;;; Rendering of links.

(defmethod vc-rational-synergy--hio-get-version ((obj vc-rational-synergy--history-object))
  "Gets the version of a rational synergy object"
  (let (result)
    (dolist (prop (vc-rational-synergy--hio-props obj) result)
      (when (eq (car prop) 'version) (setq result (cdr prop))))))

(defmethod vc-rational-synergy--hio-get-task ((obj vc-rational-synergy--history-object))
  "Gets the task of a rational synergy object"
  (let (result)
    (dolist (prop (vc-rational-synergy--hio-props obj) result)
      (when (eq (car prop) 'task) (setq result (cdr prop))))))

(defmethod vc-rational-synergy--hio-get-owner ((obj vc-rational-synergy--history-object))
  "Gets the owner of a rational synergy object"
  (let (result)
    (dolist (prop (vc-rational-synergy--hio-props obj) result)
      (when (eq (car prop) 'owner) (setq result (cdr prop))))))

(defmethod vc-rational-synergy--hio-get-status ((obj vc-rational-synergy--history-object))
  "Gets the status of a rational synergy object"
  (let (result)
    (dolist (prop (vc-rational-synergy--hio-props obj) result)
      (when (eq (car prop) 'status) (setq result (cdr prop))))))

(defmethod vc-rational-synergy--hio-get-release ((obj vc-rational-synergy--history-object))
  "Gets the release of a rational synergy object"
  (let (result)
    (dolist (prop (vc-rational-synergy--hio-props obj) result)
      (when (eq (car prop) 'release) (setq result (cdr prop))))))

(defmethod vc-rational-synergy--hio-get-shortname ((obj vc-rational-synergy--history-object))
  "Gets the name of a rational synergy object"
  (let (result)
    (dolist (prop (vc-rational-synergy--hio-props obj) result)
      (when (eq (car prop) 'name) (setq result (cdr prop))))))

(defun vc-rational-synergy-version-compare-components-rec (c1 c2 level)
  "Compares recursively components"
  (cond
   ((and (eq c1 nil) (eq c2 nil) `(0 ,level)))
   ((and (eq (cdr c1) nil)
	 (eq (cdr c2) nil)
	 (eq (car c1) 0)
	 (eq (car c2) 0)) `(0 ,level))
   ((< (car c1) (car c2)) `(-1 ,level))
   ((> (car c1) (car c2)) `(1 ,level))
   (t (vc-rational-synergy-version-compare-components-rec (if (eq (cdr c1) nil) '(0) (cdr c1))
							  (if (eq (cdr c2) nil) '(0) (cdr c2))
							  (+ 1 level)))))

(defun vc-rational-synergy-version-compare (v1 v2)
  "Compares versions, return whether it is a parallel and wheter it is next"
  (let ((v1-comp (mapcar 'string-to-number (split-string v1 "\\.")))
	(v2-comp (mapcar 'string-to-number (split-string v2 "\\."))))
    (vc-rational-synergy-version-compare-components-rec v1-comp v2-comp 0)))
	

(defun vc-rational-synergy--version< (v1 v2)
  "Expression for sorting functions depending on a t or nil expression"
  (message (prin1-to-string `(,v1 ,v2)))
  (let ((comp-result (car (vc-rational-synergy-version-compare v1 v2))))
    (message (prin1-to-string comp-result))
    (if (< comp-result 0)
	(progn
	  (message "v1 < v2")
	  t)
      (progn
	(message "v2 < v1")
	nil))))

(defun vc-rational-synergy--version-links< (link1 link2)
  "Returns whether or not link1 < link2 the links based on right-hand link"
  (vc-rational-synergy--version< (vc-rational-synergy--hio-get-version (car link1))
				 (vc-rational-synergy--hio-get-version (car link2))))

(defun vc-rational-synergy--sort-links (links)
  "Sorts the links using `vc-rational-synergy--version-links<'"
  (sort links 'vc-rational-synergy--version-links<))

(defun vc-rational-synergy--sorted-links-buffer ()
  "Returns the sorted links from a particular buffer
Uses `vc-rational-synergy-setup-history-for-buffer' in combination with
`vc-rational-synergy--sort-links'"
  (vc-rational-synergy--sort-links
   (vc-rational-synergy-setup-history-for-buffer)))

(defun vc-rational-synergy--safe-raw-string (s)
  "Creates a safe string (escape all parentheses, quotes and semi-colons)"
    (setq s (replace-regexp-in-string (regexp-quote "[") "\\[" s nil 'literal))
    (setq s (replace-regexp-in-string (regexp-quote "\"") "\\\"" s nil 'literal))
    (setq s (replace-regexp-in-string (regexp-quote ";") "\\;" s nil 'literal))
    (setq s (replace-regexp-in-string (regexp-quote "]") "\\]" s nil 'literal))
    s)


(defun vc-rational-synergy--insert-safe-raw-render (s)
  "Inserts a safe string (escape all parentheses, quotes and semi-colons)"
  (insert (vc-rational-synergy--safe-raw-string s)))

  
(defun vc-rational-synergy--raw-render-object (obj)
  "Writes the object as Graph-easy notation"
  (let* ((task (vc-rational-synergy--hio-get-task obj))
	 (task-info (vc-rational-synergy--command-history-task-info task))
	 (task-status (car task-info))
	 (task-release (car (cdr task-info))))
    (insert "[")
    (vc-rational-synergy--insert-safe-raw-render (vc-rational-synergy--hio-get-shortname obj))
    (insert " - V: ")
    (vc-rational-synergy--insert-safe-raw-render (vc-rational-synergy--hio-get-version obj))
    (insert "\\lOwner: ")
    (vc-rational-synergy--insert-safe-raw-render (vc-rational-synergy--hio-get-owner obj))
    (insert "\\lRelease: ")
    (vc-rational-synergy--insert-safe-raw-render (vc-rational-synergy--hio-get-release obj))
    (insert " - ")
    (vc-rational-synergy--insert-safe-raw-render (vc-rational-synergy--hio-get-status obj))
    (insert "\\lTask: ")
    (vc-rational-synergy--insert-safe-raw-render task)
    (insert " - ")
    (vc-rational-synergy--insert-safe-raw-render task-release)
    (insert " - ")
    (vc-rational-synergy--insert-safe-raw-render task-status)
    (insert "\\c \\c ")
    (vc-rational-synergy--insert-safe-raw-render (vc-rational-synergy--hio-name obj))

    (insert "]")))

(defun vc-rational-synergy--raw-pre-render (sorted-links name orig-buffer)
  "Creates the actual content for the rendering process"
  (message name)
  (insert (concat "graph {border: 1px solid black; label: \\l>>> Associated File: "
		  (vc-rational-synergy--safe-raw-string name)
		  " <<<\\l>>> Associated Buffer: "
		  (vc-rational-synergy--safe-raw-string (with-current-buffer orig-buffer (buffer-name)))
		  " <<< ;"
		  (if vc-rational-synergy-history-flow-south
		      "flow: south;"
		    "")
		  "}\n"))
  (message name)
  (dolist (link sorted-links)
    (vc-rational-synergy--raw-render-object (car link))
    (insert "-->")
    (vc-rational-synergy--raw-render-object (cdr link))
    (insert "\n")))

(defun vc-rational-synergy--history-diff (link)
  "performs comparison of associated versions"
  (let* ((1st-name (vc-rational-synergy--hio-name (car link)))
	 (2nd-name (vc-rational-synergy--hio-name (cdr link)))
	 (1st-buffer (get-buffer-create (concat vc-rational-synergy-buffer-name "*" 1st-name "*")))
	 (2nd-buffer (get-buffer-create (concat vc-rational-synergy-buffer-name "*" 2nd-name "*"))))
    (with-current-buffer 1st-buffer
      (save-excursion
	(pop-to-buffer (current-buffer))
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert (vc-rational-synergy--command-history-cat 1st-name))
	(setq buffer-read-only t)))
    (with-current-buffer 2nd-buffer
      (save-excursion
	(pop-to-buffer (current-buffer))
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert (vc-rational-synergy--command-history-cat 2nd-name))
	(setq buffer-read-only t)))
    (ediff-buffers 1st-buffer 2nd-buffer)))

(defun vc-rational-synergy--history-one-diff (name buffer)
  "performs comparison of associated versions"
  (let* ((1st-buffer (get-buffer-create (concat vc-rational-synergy-buffer-name "*" name "*"))))
    (with-current-buffer 1st-buffer
      (save-excursion
	(pop-to-buffer (current-buffer))
	(setq buffer-read-only nil)
	(erase-buffer)
	(insert (vc-rational-synergy--command-history-cat name))
	(setq buffer-read-only t)))
    (ediff-buffers 1st-buffer buffer)))
  

(defun vc-rational-synergy--history-action (name)
  "Invoked when pressing a button"
  (message name)
  (let* ((important-links
	  (cons '("Working buffer" "working-buffer")
		(delq nil (mapcar (lambda (l)
				    (cond ((equal name (vc-rational-synergy--hio-name (car l)))
					   (list (format "Diff from selected to %s"
							 (vc-rational-synergy--hio-name (cdr l)))
						 (vc-rational-synergy--hio-name (cdr l))))
					  ((equal name (vc-rational-synergy--hio-name (cdr l)))
					   (list (format "Diff from %s to selected"
							 (vc-rational-synergy--hio-name (car l)))
						 (vc-rational-synergy--hio-name (car l))))))
				  vc-rational-synergy--history-links))))
	 (completion-list (mapcar (lambda (i) (car (cdr i))) important-links))
	 (minibuffer-help-form (mapconcat (lambda (i) (car i)) important-links "\n"))
	 (_ (message (prin1-to-string completion-list)))
	 (counter-part (completing-read
			    "Select comparison to [tab to complete, help on C-h]: "
			    completion-list nil t)))
    (when counter-part
      (if (equal counter-part "working-buffer")
	  (vc-rational-synergy--history-one-diff name vc-rational-synergy--orig-buffer)
	(progn
	  (let (link)
	    (dolist (l sorted-links link)
	      (cond ((and (equal name (vc-rational-synergy--hio-name (car l)))
			  (equal counter-part (vc-rational-synergy--hio-name (cdr l))))
		     (setq link l))
		    ((and (equal name (vc-rational-synergy--hio-name (cdr l)))
			  (equal counter-part (vc-rational-synergy--hio-name (car l))))
		     (setq link l))))
	    (when link
	      (vc-rational-synergy--history-diff link))))))))

(defun vc-rational-synergy--transpose-to-button ()
  "Transposes whatever sensive names are found to a button"
  (let* ((regexp "\s[^~]+\~[^:]+:[^:]+:[0-9]+")
	 (end (re-search-forward regexp nil t))
	 (start (re-search-backward regexp nil t))
	 (_ (re-search-forward regexp nil t)))
    (when (and start end)
      (make-button (+ 1 start) end
		   'action (lambda (x) (vc-rational-synergy--history-action 
					(button-get x 'name)))
		   'help-echo "Difference options"
		   'name (buffer-substring (+ 1 start) end))
      (vc-rational-synergy--transpose-to-button))))
  

(defun vc-rational-synergy--history-make-buttons (buffer)
  "Transposes fields with names to buttons"
  (with-current-buffer buffer
    (goto-char 0)
    (vc-rational-synergy--transpose-to-button)))


(defun vc-rational-synergy-history ()
  "Draws a diagram using the knowledge of the history
Requires Bash, Perl and Graph-Easy"
  (interactive)
  (let* ((orig-buffer (current-buffer))
	 (buffer (vc-rational-synergy-buffer))
	 (sorted-links (vc-rational-synergy--sorted-links-buffer))
	 (perl (concat (vc-rational-synergy-unixify-path vc-rational-synergy-perl-path) "/perl"))
	 (script (concat 
		  (vc-rational-synergy-unixify-path vc-rational-synergy-graph-easy) 
		  "/bin/graph-easy"))
	 (lib (concat 
	       (vc-rational-synergy-unixify-path vc-rational-synergy-graph-easy) 
	       "/lib"))
	 (command-line `(,perl "-I" ,lib ,script))
	 (temp-file (make-temp-file "_grapheasy" nil ".txt"))
	 (file-name (buffer-file-name)))
    (message (prin1-to-string command-line))
    (message temp-file)
    (with-temp-file temp-file (vc-rational-synergy--raw-pre-render sorted-links file-name orig-buffer))
    (pop-to-buffer buffer)
    (erase-buffer)
    (make-local-variable 'vc-rational-synergy--history-links)
    (make-local-variable 'vc-rational-synergy--orig-buffer)
    (setq vc-rational-synergy--history-links sorted-links)
    (setq vc-rational-synergy--orig-buffer orig-buffer)
    (apply 'call-process (car command-line) temp-file (current-buffer) nil (cdr command-line))
    (condition-case err
	(delete temp-file)
      (error nil))
    (vc-rational-synergy--history-make-buttons buffer)))


(provide 'vc-rational-synergy-history)

;;; vc-rational-synergy-history.el ends here
