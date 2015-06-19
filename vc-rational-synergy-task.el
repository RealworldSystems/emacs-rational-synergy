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
  (let* ((raw-task (vc-rational-synergy-command-to-string '("task" "-default")))
	 ;; The task id is found as the first numeric section of
	 ;; raw-task
	 (task-id (string-to-number raw-task))
	 (result (vc-rational-synergy-command-w/format-to-list
		  `("task" "-show" "i" ,(format "%s" task-id))
		  'task-number 'task-description 'task-synopsis)))
    (when result (car result))))
	
	

;;;; Task management.

(defclass vc-rational-synergy-task ()
  ((task-id :initarg :id :reader vc-rational-synergy-task-id)
   (task-name :initarg :name :reader vc-rational-synergy-task-name)
   (task-project :initarg :project vc-rational-synergy-task-project))
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
    (make-instance 'vc-rational-synergy-task
		   :id (nth 0 raw-task)
		   :name (nth 1 raw-task)
		   :project (nth 2 raw-task))))

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
  

;;;###autoload
(defun vc-rational-synergy-select-task ()
  "Select an IBM Rational Synergy task"
  (interactive)
  (with-vc-rational-synergy
   ;; ----------
   ;; Use the command-interface to retrieve the tasks which will be
   ;; presented 'tabbable' to the user to select 1
   ;; ----------
   (let ((user (vc-rational-synergy-logged-on-user))
	 (project (vc-rational-synergy-current-project)))

     (message (format "Currently logged on user %s, project %s" user project))
     
     ;; A task can not be selected if there is no project or no user
     (cond
      ((not user) (vc-rational-synergy-message
		   "Can not select task, no user available"))
      ((not project) (vc-rational-synergy-message 
		      "This buffer has no project associated"))
      
      ;; Project is available and user is available, therefor falling
      ;; through
      (t (vc-rational-synergy--select-task-for-project user project))))))


;;;###autoload
(defun vc-rational-synergy-default-task ()
  "Retrieves the default task from Synergy."
  (interactive)
  
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
	(error "Select a task first!"))))))


(provide 'vc-rational-synergy-task)

;; vc-rational-synergy-current-project ends here
