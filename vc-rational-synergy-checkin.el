;;; vc-rational-synergy-checkin.el --- IBM Rational Synergy checkin commands
 
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

;; Supports checkin in files, directories and/or tasks

;;; Code:

(require 'vc-rational-synergy-command-to-string)
(require 'vc-rational-synergy-buffer)
(require 'vc-rational-synergy-task)

(defun vc-rational-synergy--command-file-status (file-name)
  "Acquire the native form of the file-name status, if applicable
This is a wrapper around `vc-rational-synergy-command-w/format-to-list'"
  (condition-case err
      (let ((listed (vc-rational-synergy-command-w/format-to-list
		     `("dir" ,file-name)
		     'status)))
	(message (prin1-to-string listed))
	(when listed (car (car listed))))
    (error nil)))



(defun vc-rational-synergy--command-ci (file-name comment)
  "Check in a file, using the given comment
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (let ((command-line (if comment
			  `("ci" "-c" ,comment ,file-name)
			`("ci" "-nc" ,file-name))))
    (condition-case err
	(vc-rational-synergy-command-to-string command-line)
      (error nil))))


(defun vc-rational-synergy--command-ci-task (comment)
  "Check in a file, using the given comment
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (let ((command-line (if comment
			  `("ci" "-task" "default" "-c" ,comment)
			`("ci" "-task" "default" "-nc"))))
    (condition-case err
	(vc-rational-synergy-command-to-string command-line)
      (error nil))))

(defun vc-rational-synergy--check-comment (comment)
  "Checks the comment, if the comment is not set, asks the user
if this function is called through interactive operation"
  (unless comment
    (setq comment
	  (read-string 
	   "Checkin-comment (newlines with S-RET, RET when done): ")))
  
  ;; If the comment is still empty, ask for confirmation, or if
  ;; `vc-rational-synergy-disallow-empty-comment' is set, bail
  ;; out the hard way
  (when (or (eq comment nil) (string= comment ""))
    (when vc-rational-synergy-disallow-empty-comment
      (error "Empty comments forcefully disallowed"))
    (unless (y-or-n-p "No comment: are you sure to continue committing?")
      (throw 'exit)))
  (cond ((eq nil comment) nil)
	((string= "" comment) nil)
	(t comment)))


(defun vc-rational-synergy--attempt-checking-in-task-files (comment)
  "Attempts full check-in for files, returns files not checked in"
  (delq nil
	(mapcar (lambda (file)
		  (unless (vc-rational-synergy--command-ci file comment)
		    file))
		    (vc-rational-synergy--working-task-files))))


(defun vc-rational-synergy--attempt-checking-in-directory-files (comment)
  "Attempts full check-in for all files in the current directory
If any checkin has failed, return those failures as list"
  (delq nil
	(mapcar (lambda (file)
		  (unless (vc-rational-synergy--command-ci file comment)
		    file))
		    (vc-rational-synergy--working-directory-files
		     (vc-rational-synergy--buffer-directory)))))
		

;;;###autoload
(defun vc-rational-synergy-ci-task (&optional comment)
  "Checks in the default task."
  (interactive)
  (condition-case err
      (progn
	(with-vc-rational-synergy
	 (when (vc-rational-synergy--check-buffer-assoc)
	   ;; Check if there are any buffers associated to the default
	   ;; task which might have been modified.
	   (when (vc-rational-synergy--buffers-modified-p)
		 (error "There are unsaved buffers"))
	   
	   (setq comment (vc-rational-synergy--check-comment comment))
	   
	   ;; If we get here, the actual checking in can commence
	   
	   (let ((failed-files (vc-rational-synergy--attempt-checking-in-task-files
				comment)))
	     (when failed-files
	       (error (concat "Stop task check-in, failed to check-in: "
			      (mapconcat (lambda (x) x) failed-files "; ")))))

	   (let* ((task (vc-rational-synergy-get-default-task))
		  (human (vc-rational-synergy-task-human task)))
	     
	     ;; First check in all the files which are still in working
	     ;; mode

	     ;; If we get here, then files are checked in and it becomes
	     ;; possible to actually check in the task at last
	     (if (vc-rational-synergy--command-ci-task comment)
		 (prog1 t
		   (vc-rational-synergy--revert-buffers)
		   (vc-rational-synergy-message "Task %s checked in" human))
	       (prog1 nil
		 (error (format "Failed checking in: %s" human))))))))
    (error (vc-rational-synergy-message (error-message-string err)))))


;;;###autoload
(defun vc-rational-synergy-ci-file (&optional comment)
  "Check in a file into CM Synergy.
If COMMENT is set, checking the file with given comment"
  (interactive)

  (catch 'exit

    ;; Check if the buffer is really in working status, if not,
    ;; exit and inform the user if interactive.
    (unless (vc-rational-synergy-buffer-working)
      (progn
	(when (called-interactively-p 'all)
	  (vc-rational-synergy-message "%s" "Buffer not in working mode"))
	(throw 'exit nil)))

    (setq comment (vc-rational-synergy--check-comment comment))
    
      ;; If the buffer is modified, it should really be saved, ask
      ;; the user to do this. If the user doesn't, request again
      ;; to continue saving.
    (when (called-interactively-p 'all)
      (when (buffer-modified-p)
	(if (y-or-n-p "Buffer is modified, save buffer first?")
	    (save-buffer)
	  (unless (y-or-n-p 
		   "Not saving: Data in buffer is not checked in, continue?")
	    (throw 'exit nil)))))

    (with-vc-rational-synergy
     (if (vc-rational-synergy--command-ci (buffer-file-name) comment)
	 (vc-rational-synergy-message "Succesfully checked in %s"
				      (buffer-file-name))))
    (revert-buffer nil t)))


;;;###autoload
(defun vc-rational-synergy-ci-directory (&optional comment)
  "Check in an entire directory into CM Synergy.
If COMMENT is set, checking the file with given comment"
  (interactive)
  (condition-case err
      (progn
	(with-vc-rational-synergy
	 (unless (vc-rational-synergy-buffer-directory-working)
	   (error
	    "The directory pointed to by this buffer is not in working state"))
	 
	 ;; Check if there are any buffers associated to the default
	 ;; task which might have been modified.
	 (when (vc-rational-synergy--buffers-modified-p)
	   (error "There are unsaved buffers"))
	 
	 
	 (setq comment (vc-rational-synergy--check-comment comment))
	   
	 (let ((directory-name (vc-rational-synergy--buffer-directory)))

	   ;; If we get here, the actual checking in can commence
	   
	   ;; First check in all the files which are still in working
	   ;; mode
	   (let ((failed-files (vc-rational-synergy--attempt-checking-in-directory-files
				comment)))
	     (when failed-files
	       (error (concat "Stop directory check-in, failed to check-in: "
			      (mapconcat (lambda (x) x) failed-files "; ")))))
	   
	   
	   ;; If we get here, then files are checked in and it becomes
	   ;; possible to actually check in the directory
	   (if (vc-rational-synergy--command-ci directory-name comment)
	       (prog1 t
		 (vc-rational-synergy--revert-buffers)
		 (vc-rational-synergy-message "Directory %s checked in" directory-name))
	     (prog1 nil
	       (error (format "Failed checking in: %s" directory-name)))))))
    (error (vc-rational-synergy-message (error-message-string err)))))

(provide 'vc-rational-synergy-checkin)
;;; vc-rational-synergy-checkin.el ends here
