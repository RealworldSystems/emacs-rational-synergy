;;; vc-rational-synergy-task.el --- IBM Rational Synergy task commands
 
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
;;     Geert Ribbers
;;     Henrik Joensson <henrik7205@hotmail.com>
;;     
;; Maintainer:
;;     Sjoerd van Leent <sjoerd.van.leent@realworld-systems.com>
;;
;; Homepage: http://github.com/RealworldSystems/emacs-rational-synergy
;;
;; Keywords: ibm rational synergy telelogic vc version-control

;;; Code:

(require 'vc-rational-synergy-command-to-string)
(require 'vc-rational-synergy-utilities)
(require 'vc-rational-synergy-tabular)

;;;; CCM interface.

;;;###autoload
(defun vc-rational-synergy--command-tasks (user)
  "Acquires the native form of all tasks assigned to a particular user.
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (vc-rational-synergy-command-w/format-to-list
   `("query" 
     ,(format "status='task_assigned' and resolver='%s'" user)
     "-u")
   'task-number 'task-description 'task-synopsis))


;;;###autoload
(defun vc-rational-synergy-command-default-task ()
  "Acquires the native form of a default task.
This is a wrapper around `vc-rational-synergy-command-to-string'
to be able to get a default task directly from the command"
  (condition-case err
      (progn
	(let* ((raw-task (vc-rational-synergy-command-to-string '("task" "-default")))
	       ;; The task id is found as the first numeric section of
	       ;; raw-task
	       (task-id (string-to-number raw-task))
	       (result (vc-rational-synergy-command-w/format-to-list
			`("task" "-show" "i" ,(format "%s" task-id))
			'task-number 'task-description 'task-synopsis)))
	  (when result (car result))))
      (error nil)))
	
	
(defun vc-rational-synergy--command-task-files-using-id (task-id)
  "Acquires the native form of all task files associated.
uses `vc-rational-synergy-command-w/format-to-list' to gather
all files associated to a particular task. This only works
properly if there is already an association between buffer and
a task identifier is given. If not properly satisfied, returns
nil, otherwise a list with associated task files in raw format
and status thereof"
  (condition-case err
      (let ((listed (vc-rational-synergy-command-w/format-to-list
		     `("task" "-show" "objects" "-u" ,task-id)
		     'objectname 'status)))
	;; Only return if listed is not empty
	(when listed listed))
    (error nil)))

(defun vc-rational-synergy--command-rq-object-hist-leave (name version type instance)
  "Requests the ojbect type for a particular object.
An IBM Rational Synergy object is identified by NAME, VERSION, TYPE and
INSTANCE identification."
  (let* ((query (format 
		"name='%s' and version='%s' and type='%s' and instance='%s' and is_hist_leaf()"
		name version type instance))
	 (result (vc-rational-synergy-command-w/format-to-list
		  `("query" ,query "-u")
		  'name)))
    (if result t nil)))
      



;;;; Task management.

(defclass vc-rational-synergy-task ()
  ((task-id :initarg :id :reader vc-rational-synergy-task-id)
   (task-name :initarg :name :reader vc-rational-synergy-task-name)
   (task-project :initarg :project :reader vc-rational-synergy-task-project))
  "Represents an IBM Rational Synergy task")

(defmethod vc-rational-synergy-task-human
  ((task vc-rational-synergy-task))
  "Creates a human readable variant of a `vc-rational-synergy-task' instance"
  (format "%s => %s" (vc-rational-synergy-task-id task)
	  (vc-rational-synergy-task-name task)))

;;;###autoload
(defun vc-rational-synergy-get-tasks-for-user (user)
  "Acquires tasks, represented by `vc-rational-synergy-task'"
  (let ((raw-tasks (vc-rational-synergy--command-tasks user)))
    (mapcar (lambda (raw-task)
	      (make-instance 'vc-rational-synergy-task
			     :id (nth 0 raw-task)
			     :name (nth 1 raw-task)
			     :project (nth 2 raw-task)))
	    raw-tasks)))


;;;###autoload
(defun vc-rational-synergy-get-default-task ()
  "Acquires tasks, represented by `vc-rational-synergy-task'"
  (let ((raw-task (vc-rational-synergy-command-default-task)))
    (when raw-task
      (make-instance 'vc-rational-synergy-task
		     :id (nth 0 raw-task)
		     :name (nth 1 raw-task)
		     :project (nth 2 raw-task)))))

(defun vc-rational-synergy--select-task-for-project (user project)
  "This is the actual implementation function of 
`vc-rational-synergy-select-task'"

  (let* ((tasks (vc-rational-synergy-get-tasks-for-user user))
	 (completion-list (mapcar 'vc-rational-synergy-task-id tasks))
	 (minibuffer-help-form (mapconcat 'vc-rational-synergy-task-human
					  tasks
					  "\n"))
	 (selected-task-id (completing-read
			    "Select task [tab to complete, help on C-h]: "
			    completion-list nil t)))
    (message "  !! vc-cmsyn-select-task-cli 3")
    (if (<= (length selected-task-id) 0)
	(vc-rational-synergy-message "Task not set!")
      (vc-rational-synergy-command-to-string `("task" "-default" ,selected-task-id))
      (vc-rational-synergy-message "Task set to %s!" selected-task-id))))
  

(defun vc-rational-synergy--check-buffer-assoc ()
  "Checks if the given buffer is associated to a user or project.
Only if a buffer is properly associated, task operations can be performed.

If there is no valid association, a message will be displayed using
`vc-rational-synergy-message' and nil will be returned, otherwise,
t will be returned"
  (with-vc-rational-synergy
   (let ((user (vc-rational-synergy-logged-on-user))
	 (project (vc-rational-synergy-current-project)))

     (message (format "Currently logged on user %s, project %s" user project))
     
     ;; A task can not be selected if there is no project or no user
     (cond
      ((not user) (progn
		    (vc-rational-synergy-message
		     "Can not select task, no user available")
		    nil))
      ((not project) (progn
		       (vc-rational-synergy-message 
			"This buffer has no project associated")
		       nil))
      (t t)))))
   

;;;###autoload
(defun vc-rational-synergy-select-task ()
  "Select an IBM Rational Synergy task"
  (interactive)
  (when (vc-rational-synergy--check-buffer-assoc)
    (with-vc-rational-synergy
      (vc-rational-synergy--select-task-for-project 
       (vc-rational-synergy-logged-on-user)
       (vc-rational-synergy-current-project)))))


;;;###autoload
(defun vc-rational-synergy-default-task ()
  "Retrieves the default task from Synergy."
  (interactive)
  (when (vc-rational-synergy--check-buffer-assoc)
    (with-vc-rational-synergy
     (let* ((default-task (vc-rational-synergy-get-default-task)))
       (if default-task
	   (progn
	     (when (called-interactively-p 'interactive)
	       (vc-rational-synergy-message
		(vc-rational-synergy-task-human default-task)))
	     default-task)
	 (if (called-interactively-p 'interactive)
	     (vc-rational-synergy-message "No default task selected")
	   (error "Select a task first!")))))))


;;; Task/file management.

(defclass vc-rational-synergy-task-file ()
  ((file-name :initarg :name :reader vc-rational-synergy-task-file-name)
   (file-type :initarg :type :reader vc-rational-synergy-task-file-type)
   (file-revision :initarg :rev :reader vc-rational-synergy-task-file-revision)
   (file-instance :initarg :instance :reader vc-rational-synergy-task-file-instance)
   (file-status :initarg :status :reader vc-rational-synergy-task-file-status)
   (objectname :initarg :objectname :reader vc-rational-synergy-task-file-objectname))
  "Represents an IBM Rational Synergy task file")

(defmethod vc-rational-synergy-task-is-hist-leaf
  ((obj vc-rational-synergy-task-file))
  "Checks if a given task file has a history leaf"
  (with-vc-rational-synergy
   (vc-rational-synergy--command-rq-object-hist-leave 
    (vc-rational-synergy-task-file-name obj)
    (vc-rational-synergy-task-file-revision obj)
    (vc-rational-synergy-task-file-type obj)
    (vc-rational-synergy-task-file-instance instance))))

(defclass vc-rational-synergy-task-file-with-path (vc-rational-synergy-task-file)
  ((path :initarg :path :reader vc-rational-synergy-task-file-path))
  "Represents a task file with an associated path where possible")

(defun vc-rational-synergy--assoc-task-file-with-path (task-file path)
  "If path is given, associate it with task file, otherwise return task-file"
  (if path
      (make-instance 'vc-rational-synergy-task-file-with-path
		     :name (vc-rational-synergy-task-file-name task-file)
		     :rev (vc-rational-synergy-task-file-revision task-file)
		     :type (vc-rational-synergy-task-file-type task-file)
		     :instance (vc-rational-synergy-task-file-instance task-file)
		     :status (vc-rational-synergy-task-file-status task-file)
		     :objectname (vc-rational-synergy-task-file-objectname task-file)
		     :path path)
    task-file))

(defun vc-rational-synergy--make-task-file (raw-file)
  "Creates a `vc-rational-synergy-task-file' from a RAW-FILE
RAW-FILE is taken to be a colon separated <name>:<type>:<revision>
format"
  ;; First acquire the end token, which is the instance. The
  ;; reason is that on some systems, the colon is actually a
  ;; valid file name, The regexp for the last part is: ":[^:]*$"
  ;; 
  ;; The nice thing is that this is the same for the regular
  ;; expression of the type identifier
  ;;
  ;; At the end, use the name to split the real name and version
  ;; based on tilde.
  
  (let* ((head (car raw-file))
	 (end-location (string-match ":[^:]*$" head))
	 (instance (string-to-int (substring head (+ 1 end-location))))
	 (first-part (substring head 0 end-location))
	 (snd-location (string-match ":[^:]*$" first-part))
	 (type (substring first-part (+ 1 snd-location)))
	 (name (substring first-part 0 snd-location))
	 (sep vc-rational-synergy-version-object-separator)
	 (trd-location (string-match (format "%s[^%s]*$" sep sep) name))
	 (real-name (substring name 0 trd-location))
	 (rev (substring name (+ 1 trd-location))))
    (make-instance 'vc-rational-synergy-task-file
		   :name real-name 
		   :type type 
		   :rev rev
		   :instance instance
		   :objectname head
		   :status (car (cdr raw-file)))))
			     

(defun vc-rational-synergy--get-task-files-using-id (task-id)
  "Gets the task files associated to a particular task
The TASK-ID is used to identify a particular task, given that
TASK-ID is correct, and the buffer is associated properly."
  (let ((raw-files (vc-rational-synergy--command-task-files-using-id
		    task-id)))
    (mapcar (lambda (raw-file)
	      (vc-rational-synergy--make-task-file raw-file))
	    raw-files)))
	

     

(defun vc-rational-synergy--task-files-to-buffer (task-files &optional wa-root)
  "Displays a series of task files into a buffer, if task-files is set,
otherwise displays a message that no task files have been associated"
  (let ((buffer (vc-rational-synergy-buffer)))
    (message (prin1-to-string buffer))
    (pop-to-buffer buffer)
    (erase-buffer)
    (insert (vc-rational-synergy--tabular-task-files task-files wa-root))))
 
(defun vc-rational-synergy--take-task-file-path (task-file paths)
  "Using a task file, attempts to take a path, otherwise returns nil"
  (catch 'exit
    (dolist (path paths)
      (if (string= (car path) 
		   (vc-rational-synergy-task-file-objectname task-file))
	  (throw 'exit (car (cdr path)))))))
  

(defun vc-rational-synergy--cross-assoc-task-files-paths (task-files paths)
  "Associates task files with paths, if any"
  (mapcar (lambda (task-file)
	    (vc-rational-synergy--assoc-task-file-with-path
	     task-file
	     (vc-rational-synergy--take-task-file-path task-file paths)))
	  task-files))

(defun vc-rational-synergy--get-task-files-for-default-task ()
  "Acquires the files for the default task."
  (with-vc-rational-synergy
   (let* ((task (vc-rational-synergy-get-default-task)))
     (if task
	 (let* ((task-id (vc-rational-synergy-task-id task))
		(task-name (vc-rational-synergy-task-name task)))
	   (message (format "Retrieving files for task %s..." task-name))
	   (let* ((task-files (vc-rational-synergy--get-task-files-using-id task-id))
		  (paths (vc-rational-synergy--command-paths
			  (vc-rational-synergy-current-project))))
	     (vc-rational-synergy--cross-assoc-task-files-paths 
	      task-files paths)))))))
		    

 
;;;###autoload
(defun vc-rational-synergy-show-task-files ()
  "Show the files of a particular task."
  (interactive)
  (when (vc-rational-synergy--check-buffer-assoc)
    (let ((task-files (vc-rational-synergy--get-task-files-for-default-task)))
      (if task-files
	  (vc-rational-synergy--task-files-to-buffer 
	   task-files
	   (vc-rational-synergy--command-wa-path-for-project
	    (vc-rational-synergy-current-project)))
	(vc-rational-synergy-message "No task files found")))))




;;;###autoload
(defun vc-rational-synergy-open-task-files ()
  "Open task files."
  (interactive)
  (when (vc-rational-synergy--check-buffer-assoc)
    (let ((task-files (vc-rational-synergy--get-task-files-for-default-task)))
      (dolist (tf task-files)
	(condition-case err
	    (unless (string= "dir" (vc-rational-synergy-task-file-type tf))
	      (find-file (vc-rational-synergy-task-file-path tf)))
	  (no-method-definition nil))))))


(provide 'vc-rational-synergy-task)

;; vc-rational-synergy-current-project ends here
