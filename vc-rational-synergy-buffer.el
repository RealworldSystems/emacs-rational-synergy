;;; vc-rational-synergy-buffer.el --- IBM Rational Synergy buffer

;; Copyright (C) 2015 Realworld OO Systems B.V., the Netherlands
 
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

;; These functions deal with the IBM Rational Synergy buffer

;; Code:

(require 'vc-rational-synergy-modeline)

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

(defun vc-rational-synergy--buffer-directory ()
  "Given the current buffer, acquire the directory of that buffer.
If the current buffer is a dired buffer, use the directory name the dired
buffer is visiting, otherwise, use the file being visited by the buffer,
and derive the directory. If no file is being visited either, return nil."
  (if (equal major-mode 'dired-mode) 
      (if (listp dired-directory)
	  (car dired-directory)
	dired-directory)
    (when (buffer-file-name)
      (file-name-directory (buffer-file-name)))))

(defun vc-rational-synergy-buffer-directory-status (&optional buffer-or-name)
  "Checks the status of the underlying directory of the selected buffer.
if BUFFER-OR-NAME is set, uses that buffer, otherwise uses the
current buffer.
In case that the buffer is a dired buffer, the directory visited by the
dired buffer is used."
  (interactive)

  (unless buffer-or-name (setq buffer-or-name (current-buffer)))

  (with-current-buffer buffer-or-name
    ;; There are two options now, either the buffer is visiting a file,
    ;; the approach is similar to `vc-rational-synergy-buffer-file-status'
    ;; If the buffer, on the other hand, is a dired buffer, the dired-directory
    ;; is used.

    (let ((directory-name (vc-rational-synergy--buffer-directory)))
      (unless directory-name
	(error (format "%s: %s" "This buffer does not have a file associated"
		       (buffer-name (current-buffer)))))
      (let ((st (with-vc-rational-synergy
		 (vc-rational-synergy--command-file-status directory-name))))
	(unless st
	  (error (format "%s: %s" "No valid status found for"
			 directory-name)))
	(when (called-interactively-p 'interactive)
	  (vc-rational-synergy-message
	   "Directory [%s] for buffer [%s] has status [%s]"
	   directory-name (buffer-name (current-buffer)) st))
	st))))


(defun vc-rational-synergy-buffer-file-status (&optional buffer-or-name)
  "Checks the file status of the selected buffer.
if BUFFER-OR-NAME is set, uses that buffer, otherwise uses the
current buffer."
  (interactive)

  (unless buffer-or-name (setq buffer-or-name (current-buffer)))

  (with-current-buffer buffer-or-name
    (unless (buffer-file-name)
      (error (format "%s: %s" "This buffer does not have a file associated"
		     (buffer-name (current-buffer)))))
    (let ((st (with-vc-rational-synergy
	       (vc-rational-synergy--command-file-status (buffer-file-name)))))
      (unless st
	(error (format "%s: %s" "No valid status found for"
		       (buffer-file-name))))
      (when (called-interactively-p 'interactive)
	(vc-rational-synergy-message
	 "File [%s] for buffer [%s] has status [%s]"
	 (buffer-file-name) (buffer-name (current-buffer)) st))
      st)))

(defun vc-rational-synergy-buffer-working (&optional buffer-or-name)
  "Checks if the buffer has a working file status"
  (string= "working"
	   (vc-rational-synergy-buffer-file-status buffer-or-name)))

(defun vc-rational-synergy-buffer-integrate (&optional buffer-or-name)
  "Checks if the buffer has an integrate file status"
  (string= "integrate"
	   (vc-rational-synergy-buffer-file-status buffer-or-name)))

(defun vc-rational-synergy-buffer-directory-working (&optional buffer-or-name)
  "Checks if the buffer has a working file status"
  (string= "working"
	   (vc-rational-synergy-buffer-directory-status buffer-or-name)))

(defun vc-rational-synergy-buffer-directory-integrate (&optional buffer-or-name)
  "Checks if the buffer has an integrate file status"
  (string= "integrate"
	   (vc-rational-synergy-buffer-directory-status buffer-or-name)))


(defun vc-rational-synergy--synergized-buffers ()
  "Returns all buffers part of the default task." 
    (let* ((files (vc-rational-synergy--working-task-files)))
      (delq nil
	    (mapcar (lambda (b)
		      (with-current-buffer b
			(when (and (boundp 'vc-cmsyn-mode) vc-cmsyn-mode) b)))
		    (buffer-list)))))

(defun vc-rational-synergy--revert-buffers ()
  "Checks if the default task associates unsaved buffers."
  (dolist (buffer (vc-rational-synergy--synergized-buffers))
    (with-current-buffer buffer
      (revert-buffer nil t))))


(defun vc-rational-synergy--buffers-modified-p ()
  "Checks if the default task associates unsaved buffers."
  (catch 'exit
    (let* ((synergized (vc-rational-synergy--synergized-buffers)))
      (dolist (buffer synergized)
	(with-current-buffer buffer
	  (when (buffer-modified-p) (throw 'exit t)))))))


(defun vc-rational-synergy-buffer ()
  "Return the output-buffer for the ccm process.
If the buffer does not exist yet, create it within the current frame"
  (interactive)
  
  (save-excursion
    (selected-frame)
    (let ((buffer (get-buffer-create vc-rational-synergy-buffer-name)))
      (with-current-buffer buffer
	(setq truncate-lines t)
	(setq auto-hscroll-mode t))
      buffer)))


(defun vc-cmsyn-check-status-buffer ()
  "Checks writable/readonly compared with file-attribs"
  (when (buffer-file-name)
    (cond
     ((and (file-writable-p (buffer-file-name)) buffer-read-only)
      (toggle-read-only))
     ((and (not (file-writable-p (buffer-file-name))) (not buffer-read-only))
      (toggle-read-only))
     )))

(provide 'vc-rational-synergy-buffer)

;; vc-rational-synergy-buffer.el ends here
