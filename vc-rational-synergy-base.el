;;; vc-rational-synergy-base.el --- IBM Rational Synergy integration for Emacs
 
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
(require 'vc-rational-synergy-authentication)
(require 'vc-rational-synergy-modeline)

(require 'vc-rational-synergy-administration-customization)
(require 'vc-rational-synergy-user-customization)


;;;###autoload
(defun vc-cmsyn-co-file ()
  "Check out a file from CM Synergy.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : checks task set before trying to check-out
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  ;; ----------
  ;; Don't proceed when modified
  ;; ----------
  (when (buffer-modified-p) (error "This buffer is modified, please revert or save first."))
  ;; ----------
  ;; ok, proceed
  ;; ----------
  (let*
      (
       (l-filename (buffer-file-name))
       (l-message (format "Starting check out of %s..." l-filename))
       l-proc
       )
    (vc-cmsyn-run-command l-message (format "co %s" (vc-rational-synergy-platformify-path l-filename)) 'vc-cmsyn-sentinel-co-file nil t)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-co-directory ()
  "Check out a directory from CM Synergy.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : checks task set before trying to check-out, same as co file but dir-name is constructed
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  ;; ----------
  ;; Don't proceed when modified
  ;; ----------
  (when (buffer-modified-p) (error "This buffer is modified, please revert or save first."))
  ;; ----------
  ;; ok, proceed
  ;; ----------
  (let*
      (
       (l-filename	      (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
       (l-message (format "Starting check out of directory %s..." l-filename))
       l-proc
       )
    (vc-cmsyn-run-command l-message (format "co %s" (vc-rational-synergy-platformify-path l-filename)) 'vc-cmsyn-sentinel-co-directory nil t)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-undo-co-file ()
  "Undo file checkout from CM Synergy.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let*
      (
       (l-filename (buffer-file-name))
       (l-message (format "Starting undo check out of %s..." l-filename))
       l-proc
       )
    (vc-cmsyn-run-command l-message (format "unuse -replace -delete %s" (vc-rational-synergy-platformify-path l-filename)) 'vc-cmsyn-sentinel-undo-co-file nil t)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-undo-co-directory ()
  "Undo directory checkout from CM Synergy.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let*
      (
       (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
       (l-message (format "Starting undo check out of %s..." l-filename))
       l-proc
	)
    (vc-cmsyn-run-command l-message (format "unuse -replace -delete %s" (vc-rational-synergy-platformify-path l-filename)) 'vc-cmsyn-sentinel-undo-co-directory nil t)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-register-file (&optional p-dont-check-task)
  "Creates the current file in CM Synergy, only asks the user for a type and version if configured to do so.
  Author        : Realworld Systems (GR).
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-rational-synergy-check-session)
  (let* ((l-type	      (when vc-rational-synergy-query-create-file-type (read-string "Type of the file: " "")))
	 (l-version	      (when vc-rational-synergy-query-create-file-version (read-string "Version of the file: " "")))
	 (l-filename	      (buffer-file-name))
	 (l-create-command    (if l-type (format "create -type %s %s" l-type (vc-rational-synergy-platformify-path l-filename)) (format "create %s" (vc-rational-synergy-platformify-path l-filename))))
	 (l-attr-command      (when l-version (format "attr -modify version -value %s %s" l-version (vc-rational-synergy-platformify-path l-filename))))
	 (l-message	      (format "Starting registration of %s..." l-filename))
	 )
    (vc-cmsyn-run-command l-message l-create-command 'vc-cmsyn-sentinel-register-file nil (not p-dont-check-task))
    (when l-attr-command (vc-cmsyn-run-command "Setting version..." l-attr-command)))
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-create-directory (&optional p-directory p-type)
  "Creates the current directory in CM Synergy, *with* files in it, evt. recursive.
  Author        : Realworld Systems (GR).
  Date          : Apr/2003
  Parameters    : P-DIRECTORY: if supplied, this directory will be registered, otherwise dir will be current directory.
  Returns       : "
  (interactive)
  (vc-rational-synergy-check-session)
  (let*
      (
       (l-type			(or p-type (when vc-rational-synergy-query-create-file-type (read-string "Type of the files in the Directory: " ""))))
       (l-version		(when vc-rational-synergy-query-create-file-version (read-string "Version of the Directory: " "")))
       (l-dir			(or p-directory
				    (if (equal major-mode 'dired-mode)
					(if (listp dired-directory) (car dired-directory) dired-directory)
				      (file-name-directory (buffer-file-name)))))
       (l-attr-command		(when l-version (format "attr -modify version -value %s %s" l-version (vc-rational-synergy-platformify-path l-dir))))
       (l-recursive-p		(or p-directory (y-or-n-p (format "Recursive Register %s?" l-dir))))
       (l-files			(delq nil (directory-files l-dir t "\\(^[^\\.]\\|^\\.[^.]\\)")))
       l-create-command l-string l-file l-ok?
       )
    ;; ----------
    ;; 1st this directory
    ;; ----------
    (mapcar (lambda (p-regexp)
	    (when (string-match p-regexp l-dir) (error "Directory %s should not be registered in Synergy according to a filter defined in `vc-rational-synergy-register-directory-and-files-filter'" l-dir))
	    ) vc-rational-synergy-register-directory-and-files-filter)
    (message "Registering Directory: -%s-..." l-dir)
    (setq l-create-command	(format "create -type dir %s" (vc-rational-synergy-platformify-path l-dir)))
    (setq l-string (vc-rational-synergy-command-to-string l-create-command)) ;; have to do this sync, because of multipe actions
    (save-excursion (set-buffer (vc-cmsyn-buffer)) (goto-char (point-max)) (insert "\n" l-string "\n"))
    (when (string-match vc-cmsyn-warning-error-output-regexp l-string) (error "Failed Registering %s" l-dir))
    (dolist (i-file l-files)
      ;; ----------
      ;; filter the files
      ;; ----------
      (setq l-ok? t)
      (mapcar (lambda (p-regexp)
		(when (string-match p-regexp i-file)
		  (setq l-ok? nil))
		) vc-rational-synergy-register-directory-and-files-filter)
      (when l-ok?
	(if (file-directory-p i-file)
	    (when l-recursive-p
	      (vc-cmsyn-create-directory i-file l-type) ;; recursive call
	      )
	  (setq l-file (vc-rational-synergy-platformify-path i-file))
	  (message "Registering: %s" l-file)
	  (setq l-create-command
		(if l-type (format "create -type %s %s" l-type l-file)
		  (format "create %s" l-file))) ;; type determined from extension
	  (message "Registering: %s" l-file)
	  (setq l-string (vc-rational-synergy-command-to-string l-create-command)) ;; have to do this sync, because of multipe actions
	  (save-excursion (set-buffer (vc-cmsyn-buffer)) (goto-char (point-max)) (insert "\n" l-string "\n"))
	  (when (string-match vc-cmsyn-warning-error-output-regexp l-string) (error "Failed Registering %s, see Synergy output-buffer for details" l-dir))
	  (when l-version
	    (setq l-attr-command (format "attr -modify version -value %s %s" l-version l-file))
	    (setq l-string (vc-rational-synergy-command-to-string l-attr-command))
	    (save-excursion (set-buffer (vc-cmsyn-buffer)) (goto-char (point-max)) (insert "\n" l-string "\n"))
	    (when (string-match vc-cmsyn-warning-error-output-regexp l-string) (error "Failed Setting version %s at %s" l-version l-file))
	    )
	  (when vc-rational-synergy-register-checks-in-p
	    (message "Checking in: %s" l-file)
	    (setq l-string (vc-rational-synergy-command-to-string (format "ci -c \"Checked in new file after registering\" %s" l-file)))
	    )
	  )
	)
      )
    (message "Registering Directory: -%s-...done" l-dir)
    (vc-cmsyn-show-output-buffer)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-ci-file (&optional p-comment-string p-dont-check-task)
  "Check in a file into CM Synergy.
  Date          : Apr/2003
  Parameters    : optional P-COMMENT-STRING: comment-string for check-in, so no query will be performed for it.
  Returns       : 
  Methodology   : checks task set before trying to check-in
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when (buffer-modified-p)
    (if (y-or-n-p "Buffer is modified, save buffer first?")
	(save-buffer)
      (message "Current buffer is modified, if you kill the buffer after checkin your changes will be lost!")
      (run-with-timer 3 nil 'message "")
      )
    )
  (vc-rational-synergy-check-session)
  (let*
      (
       (l-filename  (buffer-file-name))
       (l-comment   (or p-comment-string (read-string "Checkin-comment (newlines with S-ret, RET when done): ")))
       (l-message   (format "Starting check in of %s..." l-filename))
       (l-command   (if (zerop (length l-comment))
			(format "ci -nc %s" (vc-rational-synergy-platformify-path l-filename))
		      (format "ci -c \"%s\" %s" l-comment (vc-rational-synergy-platformify-path l-filename))))
       l-proc
       )
    (vc-cmsyn-run-command l-message l-command  'vc-cmsyn-sentinel-ci-file nil (not p-dont-check-task))
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-ci-directory ()
  "Directory checkin to CM Synergy.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when (buffer-modified-p)
    (if (y-or-n-p "Buffer is modified, save buffer first?")
	(save-buffer)
      (message "Current buffer is modified, if you kill the buffer after checkin your changes will be lost!")
      (run-with-timer 3 nil 'message "")
      )
    )
  (let*
      (
       (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
       (l-comment (read-string "Checkin-comment (newlines with S-ret, RET when done): "))
       (l-message (format "Starting Check in of %s..." l-filename))
       (l-command   (if (zerop (length l-comment))
			(format "ci -nc %s" (vc-rational-synergy-platformify-path l-filename))
		      (format "ci -c \"%s\" %s" l-comment (vc-rational-synergy-platformify-path l-filename))))
       l-proc
       )
    (vc-cmsyn-run-command l-message l-command 'vc-cmsyn-sentinel-ci-directory nil t)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-history-file-graphics ()
  "Display file history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (buffer-file-name))
	 (l-command (format "history %s -g" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command 'vc-cmsyn-sentinel-history-graphics)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-history-file-details ()
  "Display file history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (buffer-file-name))
	 (l-command (format "history %s" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-history-directory-graphics ()
  "Display directory history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
	 (l-command (format "history %s -g" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command 'vc-cmsyn-sentinel-history-graphics)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-history-directory-details ()
  "Display directory history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
	 (l-command (format "history %s" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-ci-task ()
  "Checks in the default task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : "
  (interactive)
  (vc-rational-synergy-check-session)
  (let* 
      ((l-message "Starting check in Task...")
       (l-comment (read-string "Checkin-comment (newlines with S-RET, RET when done): " ))
       l-proc
       )
    (vc-cmsyn-run-command l-message (format "task -checkin default -comment \"%s\"" l-comment) 'vc-cmsyn-sentinel-ci-task nil t)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-show-default-task ()
  "Display default task in the work buffer.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : When configured to do so, iconifies frame so the CM Synergy gui becomes visible
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (vc-cmsyn-run-command "Retrieving Current Default Task" "task -default")
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-show-task-files ()
  "Display default task in the work buffer.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : When configured to do so, iconifies frame so the CM Synergy gui becomes visible
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
;;   (vc-cmsyn-show-buffer)
  (let* 
      (
       (l-string (vc-cmsyn-default-task t)) ;; error when no task set
       (l-list (split-string l-string ":"))
       (l-task (car l-list)) ;; the nr.
       )
    (vc-cmsyn-run-command (format "Retrieving files for task %s..." l-string) (format "task -show objects -f \"%%objectname\" %s" l-task))
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-open-task-files ()
  "Load the differences of this task.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-rational-synergy-check-session)
  (message "Retrieving files for task ...")
  (let* 
      (
       (l-string	      (vc-cmsyn-default-task t)) ;; error when no task set or failed to retrieve
       (l-dummy		      (progn (message "Retrieving files for task %s..." l-string) l-string))
       (l-list		      (split-string l-string ":"))
       (l-task		      (car l-list)) ;; the nr.
       (l-objects-string      (vc-rational-synergy-command-to-string (format "task -show objects -f \"%%objectname@:@%%status\" %s" l-task)))
       (l-lines		      (split-string l-objects-string "\n"))
       (l-objects-list	      (delq nil
				    (mapcar (lambda (p-line)
					      ;; ----------
					      ;; format is: <nr>) <name>~version:type:instance@:@status
					      ;; ----------
					      (let* ((l-filename      (nth 1 (split-string p-line)))
						     (l-parts	      (split-string l-filename "@:@"))
						     (l-object-name   (car l-parts))
						     (l-name-parts    (split-string l-object-name ":"))
						     (l-objectversion (split-string (car l-name-parts) vc-rational-synergy-version-object-separator))
						     (l-file-leaf     (car l-objectversion))
						     (l-version	      (nth 1 l-objectversion))
						     (l-extension     (file-name-extension l-file-leaf))
						     (l-type-data     (when (not (equal l-extension "dir")) (vc-cmsyn-patch-type-data-for-file-type l-extension)))
						     (l-object-query  (when l-type-data (format "\"name= '%s'  and version= '%s' and type='%s' and instance='%s' and is_hist_leaf()\"" l-file-leaf l-version (nth 1 l-name-parts) (nth 2 l-name-parts))))
						     (l-latest-test   (when l-type-data (vc-rational-synergy-command-to-string (format "query %s" l-object-query))))
						     (l-latest-p      (when l-type-data (not (zerop (length l-latest-test)))))
						     )
						(when l-latest-p l-object-name)
						)
					      )
					    l-lines
					    )
				    )
			      )
       l-type l-type-data l-project-elements l-patch-buffer l-patch-buffer-list l-post-code-insert-fn
       l-output-string l-load-function l-filename l-path l-list l-project
       )
    (dolist (i-object-name l-objects-list)
      (setq l-list (vc-cmsyn-object-name-2-path i-object-name nil t l-project))
      (setq l-path (nth 0 l-list))
      (when (not l-project) (setq l-project (nth 1 l-list)))
      (when l-path (find-file l-path))
      )
    (message "Retrieving files for task %s...done" l-string)
    )
  (vc-rational-synergy-check-session-pause)
  )

(defun vc-cmsyn-default-task (&optional p-error-p)
  "Retrieves the default task from Synergy.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : P-ERROR-P: if t an error will be generated if no task set
  Returns       : string, nil or generates error"
  (let* 
      (
       (l-string	      (vc-rational-synergy-command-to-string "task -default"))
       (l-list		      (split-string l-string ":"))
       (l-error		      (when (and p-error-p
					 (or (<= (length l-list) 1) (string-match vc-cmsyn-warning-error-output-regexp (car l-list))))
				(error "Failed retrieving CM Synergy task: %s" l-string)))
       )
    (when  (and p-error-p 
		(<= (length l-list) 1))
      (error "Select a task first!"))
    (sub l-string "\n" "") ;; chop off the newline
    )
  )


;;;###autoload
(defun vc-cmsyn-properties ()
  "Display the properties of the file in the work buffer."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* ((l-filename (buffer-file-name))
	 (l-prop-command (format "prop %s" (vc-rational-synergy-platformify-path l-filename))))
;;     (vc-cmsyn-show-buffer)
    (vc-cmsyn-run-command "Properties..." l-prop-command))
  (vc-rational-synergy-check-session-pause)
  )

(defun vc-cmsyn-create-task ()
  "Create a new task"
  (interactive)
  (vc-rational-synergy-check-session)
  (vc-cmsyn-report "Starting create Task...")
  (let*
      (
       (l-proc (start-process vc-cmsyn-exe-name (vc-cmsyn-buffer)
					    vc-cmsyn-exe-name "create_task" "-g")))
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Utility Functions

(defvar vc-cmsyn-process-start-mark
  nil
  "*buffer-local var used for storing begin of process in the vc-cmsyn output-buffer.
  Date          : Aug/2003
  Author        : Realworld Systems (GR)."
  )
(make-variable-buffer-local 'vc-cmsyn-process-start-mark)

(defun vc-cmsyn-run-command (p-info p-command &optional p-sentinel p-filter p-check-task-set-p)
  "Call the CM Synergy executable with the parameter-line P-COMMAND, supplying info with P-INFO.
  Date          : Apr/2003
  Parameters    : P-INFO: Message to report
                  P-COMMAND: part of command-line after the name of the executable
  Returns       : 
  Author        : Realworld Systems (GR)."
  ;; ----------
  ;; check task set, if applicable
  ;; ----------
  (and vc-rational-synergy-check-default-task-set-p p-check-task-set-p (vc-cmsyn-check-task-set t))
  (let* 
      ((l-sentinel (or p-sentinel 'vc-cmsyn-sentinel))
       (l-current-buffer (current-buffer))
       (l-proc-buffer (vc-cmsyn-buffer))
       l-proc)
    (save-excursion
      (set-buffer l-proc-buffer)
      (goto-char (point-max))
      (setq vc-cmsyn-process-start-mark (point-marker))	;; buffer-local variable
      )
    (vc-cmsyn-report (format "\n%s\n" p-info))
    (setq comint-output-filter-functions
	  (push 'shell-strip-ctrl-m
		comint-output-filter-functions))
    (setq l-proc (apply 'start-process vc-cmsyn-exe-name l-proc-buffer
			vc-cmsyn-exe-name (vc-cmsyn-split-string-carefully p-command))) ;; the arguments need to be a list since emacs20
    (vc-cmsyn-map-proc-and-buffer l-proc l-current-buffer)
    (set-process-sentinel l-proc l-sentinel)
    (set-process-filter l-proc (or p-filter 'vc-cmsyn-process-filter))
    l-proc
    )
  )

(defun vc-cmsyn-split-string-carefully (p-string &optional p-separators)
  "splits string P-STRING such that parts within \" \" inside the string will stay 1 unit.
  Author        : Realworld Systems (MO)
  Date          : Jan/2005
  Parameters    : P-STRING: string to split
  Returns       : list with string-parts"
  (let* 
      (
       (l-string-to-split p-string)
       (l-splits)
       )
    (while (not (zerop (length l-string-to-split)))
      (if (string-match "^\\([^\"]*\\)\\(\"[^\"]*\"\\)\\(.*\\)" l-string-to-split)
	  (progn
	    (setq l-splits (nconc l-splits
				  (save-match-data (split-string (match-string 1 l-string-to-split) p-separators))
				  (list (match-string 2 l-string-to-split))
				  ))
	    (setq l-string-to-split (match-string 3 l-string-to-split))
	    )
	(setq l-splits (nconc l-splits (save-match-data (split-string l-string-to-split p-separators))))
	(setq l-string-to-split nil)
	)
      )
    l-splits
    )
  )

(defun vc-cmsyn-process-filter (p-proc p-string)
  "Insert the output string in process-buffer, at the end.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROC: running proc
                  P-STRING: outputstring
  Returns       : -"
  (set-buffer (vc-cmsyn-buffer))
  (goto-char (point-max))
  (insert p-string)
  )

(defvar vc-cmsyn-buffer-frame
  nil
  "*The frame with the cmsyn-output-buffer.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  )

(defun vc-cmsyn-buffer (&optional p-stay-in-current-frame)
  "Return the output-buffer for the ccm process: create if not there yet, will create in separate frame.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : ccm outputbuffer"
  (save-excursion
    (let* 
	(
	 (l-current-frame	(selected-frame))
	 (l-buffer		(get-buffer-create vc-rational-synergy-buffer-name))
	 )
      l-buffer
      )
    )
  )

(defun vc-cmsyn-buffer-insert (p-message)
  "Insert message P-MESSAGE.in the vc-cmsyn output buffer
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-MESSAGE: message to insert
  Returns       : "
  (with-current-buffer (vc-cmsyn-buffer)
    (goto-char (point-max))
    (insert (format "\n%s\n" p-message))
    )
  )

(defun vc-cmsyn-report (p-message)
  "Report the message in minibuffer and in output-buffer.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-MESSAGE: message to report
  Returns       : "
  (message p-message)
  (vc-cmsyn-buffer-insert p-message)
  )

(defvar vc-cmsyn-proc-buffer-mappings
  nil
  "*Alist with mappings of filter-proc with the buffer it should work on.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  )

(defun vc-cmsyn-map-proc-and-buffer (p-proc p-buffer)
  "Store parameters in a cons in alist `vc-cmsyn-proc-buffer-mappings' for later retrieval in sentinel or filter for proc.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Paramehters    : P-BUFFER: Buffer to be retrieved within sentinel or filter for proc
                  P-PROC: key for the alist
  Returns       : -"
  (push (cons p-proc p-buffer) vc-cmsyn-proc-buffer-mappings)
  )

(defun vc-cmsyn-get-buffer-mapped-to-proc (p-proc)
  "Get the buffer that was mapped in combination with proc P-PR0C in variable `vc-cmsyn-proc-buffer-mappings'.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROC: The key for the mapping
  Returns       : buffer or nil"
  (cdr (assoc p-proc vc-cmsyn-proc-buffer-mappings))
  )

(defun vc-cmsyn-clear-buffer-mapping-for-proc (p-proc)
  "Remove the entry for proc P-PROC.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROC: Key for the buffer-proc-mapping
  Returns       : -"
  (setq vc-cmsyn-proc-buffer-mappings (assq-delete-all p-proc vc-cmsyn-proc-buffer-mappings))
  )


(defun vc-cmsyn-sentinel (p-process p-what &optional p-dont-popup-process-buffer)
  "Function running at the end of a ccm process, called directly as sentinel, *or* from specific sentinels for specific funtions.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
                  P-DONT-POPUP-PROCESS-BUFFER: if non-nil the ccm output-buffer will not be shown
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       )
    (set-buffer l-output-buffer)
    (goto-char (point-max))
    (vc-cmsyn-dos2unix)
    ;; ----------
    ;; and clear the buffer-proc-mapping
    ;; ----------
    (vc-cmsyn-clear-buffer-mapping-for-proc p-process)
    (vc-cmsyn-report (format "CM Synergy Command %s" p-what))
    (when (and vc-rational-synergy-command-end-jump-to-output-buffer-p
	       (not p-dont-popup-process-buffer)
	       )
      (vc-cmsyn-show-output-buffer)
      )
    (set-buffer l-buffer)
    (vc-cmsyn-update-modeline) ;; and this also sets status-buffer!
    )
  )

(defun vc-cmsyn-show-output-buffer ()
  "Switch to the output-buffer and put cursor at the end, this may be to the separate frame or within the current frame.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (let* 
      (
       (l-proc-buffer (vc-cmsyn-buffer))
       (l-win (get-buffer-window l-proc-buffer 0))
       (l-frame (when l-win (window-frame l-win)))
       )
    (if l-frame
	(select-frame-set-input-focus l-frame)
      (pop-to-buffer l-proc-buffer)
      )
    (goto-char (point-max))
    )
  )

(defun vc-cmsyn-sentinel-co-file (p-process p-what &optional p-dont-check-parallel-versions)
  "Function running at the end of the ccm co process for a file, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
		  P-DONT-CHECK-PARALLEL-VERSIONS: if non-nil the parallel-version-check is skipped: used when this sentinel is
		    called from another, instead of directly as sentinel.
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       )
    ;; ----------
    ;; check parallel versions
    ;; ----------
    (when (not p-dont-check-parallel-versions)
      (save-excursion
	(set-buffer l-output-buffer)
	(when (re-search-backward "Starting check out of" nil t)
	  (when (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	    (x-popup-dialog t (list (match-string 0) (cons "Ok" t)))
	    )
	  )
	)
      )
    (set-buffer l-buffer)
    (when (string-match "finished" p-what) ;; carefull with blanks and newlines in p_what!
      (revert-buffer t t) ;; get the version that was checked out from CM Synergy
      ;; ----------
      ;; evt. reset the directory-status immediately again: other files in
      ;; the directory may be checked out for other tasks, thus it's better
      ;; not to have the directory marked for check-out for *this* task (which
      ;; is the default behaviour for personal work-areas)
      ;; When the directory is not checked out, this will just give the
      ;; message "Object '...' is already in the 'released' state.", no
      ;; problem so we don't test for being checked out 1st!
      ;; ----------
      (when vc-rational-synergy-auto-check-in-directory-p
	(let* 
	    (
	     (l-filename (vc-rational-synergy-platformify-path (buffer-file-name)))
	     (l-dir (file-name-directory l-filename))
	     (l-proc (start-process
		      "vc-cmsyn"
		      " *SYNERGY-AUTO-CHECK-IN*"
		      vc-cmsyn-exe-name
		      (format "ci -c \"Checked in automatically after co of %s by emacs/vc-cmsyn\" %s" l-filename l-dir)
		      )
		     )
	     )
	  )
	)
      )
    (vc-cmsyn-sentinel p-process p-what) ;; cleanup, evt. remove C-m's
    (message "Ready checking out file %s" (buffer-file-name l-buffer))
    )
  )

(defun vc-cmsyn-sentinel-register-file (p-process p-what)
  "Function running at the end of the ccm co process for a file, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       )
    (set-buffer l-buffer)
    (when (equal p-what "finished")
      (find-file (buffer-name l-buffer)) ;; get the version that was checked out from CM Synergy
      )
    ;; ----------
    ;; And ci this file now that it's registered
    ;; ----------
    (if vc-rational-synergy-register-checks-in-p
	(vc-cmsyn-ci-file "1st Checkin after creation") ;; this also pops to output-buffer and checks status buffer
      ;;       (vc-cmsyn-check-status-buffer) ==> via update-mode-line in (vc-cmsyn-sentinel)
      (vc-cmsyn-sentinel p-process p-what) ;; cleanup, evt. remove C-m's
      (message "%s is registered, but not checked in yet!" (buffer-file-name))
      )
    (message "Ready registering %s" (buffer-file-name l-buffer))
    )
  )

(defun vc-cmsyn-sentinel-co-directory (p-process p-what)
  "Function running at the end of the ccm co process for a directory, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       l-dir l-string
       )
    (set-buffer l-buffer)
    ;; ----------
    ;; check parallel versions for 1 of the files
    ;; ----------
    (save-excursion
      (set-buffer l-output-buffer)
      (goto-char (point-max))
      (while (re-search-backward "Starting check out " vc-cmsyn-process-start-mark t))
      (while (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	(setq l-string (format "%s; %s" l-string (match-string 0)))
	)
      (when (> (length l-string) 0) (x-popup-dialog t (list l-string (cons "Ok" t))))
      )
    (if (buffer-file-name)
	(progn
	  (setq l-dir (file-name-directory (buffer-file-name)))
	  (vc-cmsyn-sentinel-co-file p-process p-what t)) ;; don't check parallel versions
      (setq l-dir (if (consp dired-directory) (car dired-directory) dired-directory))
      (revert-buffer) ;; just refresh, this is a directory
      )
    (vc-cmsyn-sentinel p-process p-what) ;; cleanup, evt. remove C-m's, mode-line refresh
    (message "Ready checking out directory %s" l-dir)
    )
  )

(defun vc-cmsyn-sentinel-ci-file (p-process p-what)
  "Function running at the end of the ccm ci process for a file, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       )
    ;; ----------
    ;; check parallel versions
    ;; ----------
    (save-excursion
      (set-buffer l-output-buffer)
      (goto-char (point-max))
      (when (re-search-backward "Starting check in of" vc-cmsyn-process-start-mark t)
	(when (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	  (x-popup-dialog t (list (match-string 0) (cons "Ok" t)))
	  )
	)
      )
    (set-buffer l-buffer)
    (revert-buffer t t)
    (vc-cmsyn-sentinel p-process p-what)
    (message "Ready checking in file %s" (buffer-file-name l-buffer))
    )
  )

(defun vc-cmsyn-sentinel-undo-co-file (p-process p-what)
  "Function running at the end of the ccm ci process for a file, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       )
    (set-buffer l-buffer)
    (revert-buffer t t) ;; Synergy has rolled back the file, make visible
    (vc-cmsyn-sentinel p-process p-what)
    (message "Ready undoing check out of  %s" (buffer-file-name l-buffer))
    )
  )

(defun vc-cmsyn-sentinel-ci-directory (p-process p-what)
  "Function running at the end of the ccm ci process for a directory, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       (l-string "")
       )
    ;; ----------
    ;; check parallel versions for 1 of the files
    ;; ----------
    (save-excursion
      (set-buffer l-output-buffer)
      (goto-char (point-max))
      (while (re-search-backward "Starting check in " vc-cmsyn-process-start-mark t))
      (while (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	(setq l-string (format "%s; %s" l-string (match-string 0)))
	)
      (when (> (length l-string) 0) (x-popup-dialog t (list l-string (cons "Ok" t))))
      )
    (save-excursion
      (set-buffer l-buffer)
      (when (not (buffer-file-name))
	(revert-buffer) ;; just refresh, this is a directory
	)
      )
    (vc-cmsyn-check-status-buffers)
    (vc-cmsyn-clear-buffer-mapping-for-proc p-process)
    (message "Ready checking in directory")
    )
  )

(defun vc-cmsyn-sentinel-undo-co-directory (p-process p-what)
  "Function running at the end of the ccm undo co process for a directory, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       (l-string "")
       l-revert-buffer
       )
    ;; ----------
    ;; find the revert-buffer, either the directory of the buffer where
    ;; the command was called, or the buffer itself
    ;; ----------
    (save-excursion 
      (set-buffer l-buffer)
      (if (not (buffer-file-name))
	  (setq l-revert-buffer l-buffer ) ;; just refresh, this is a directory
	(setq l-revert-buffer (find-buffer-visiting (file-name-directory (buffer-file-name))))
	)
      (when l-revert-buffer
	(save-excursion
	  (set-buffer l-revert-buffer)
	  (revert-buffer t t)
	  )
	)
      )
    (vc-cmsyn-clear-buffer-mapping-for-proc p-process)
    (message "Ready undoing check out of directory")
    )
  )

(defun vc-cmsyn-sentinel-ci-task (p-process p-what) 
  "Function running at the end of the ccm ci process for a task, called directly as sentinel, calls the general `vc-cmsyn-sentinel'.
  Date          : Apr/2003
  Parameters    : P-PROCESS: the ccm process 
                  P-WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-process))
       (l-output-buffer (process-buffer p-process))
       (l-string "")
       )
    ;; ----------
    ;; check parallel versions for 1 of the files
    ;; ----------
    (save-excursion
      (set-buffer l-output-buffer)
      (goto-char (point-max))
      (while (re-search-backward "Starting check in " vc-cmsyn-process-start-mark t))
      (while (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	(setq l-string (format "%s; %s" l-string (match-string 0)))
	)
      (when (> (length l-string) 0) (x-popup-dialog t (list l-string (cons "Ok" t))))
      )
    (save-excursion
      (set-buffer l-buffer)
      (when (not (buffer-file-name))
	(revert-buffer) ;; just refresh, this is a directory
	)
      )
    ;; ----------
    ;; now update status of all vc-cmsyn-buffers!
    ;; ----------
    (vc-cmsyn-check-status-buffers)
    (vc-cmsyn-clear-buffer-mapping-for-proc p-process)
    (message "Ready checking in task")
    )
  )

(defun vc-cmsyn-sentinel-history-graphics (p-process p-what)
  "Function running when the ccm process for displaying graphical history information is done.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROCESS: ccm process
                  P-WHAT: change in process
  Returns       : "
  (vc-cmsyn-sentinel p-process p-what t) ;; standard finish, but don't popup the process-buffer, would cover the graphics-history-output
  )

(defun vc-cmsyn-sentinel-start-developers-gui (p-process p-what)
  "Function running when the ccm login process is done.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROCESS: ccm process
                  P-WHAT: change in process
  Returns       : "
  (message "!! vc-cmsyn-sentinel-start-developers-gui 1")
  (save-excursion
    (let* 
	(
	 (l-string (vc-rational-synergy-command-to-string "status")) ;; find out ccm_addr cli
	 (l-ccm-addr (progn (when (string-match  "^Command[ \t]+Interface[ \t]+@ \\(.*\\)[ \t]*\\((current session)\\)?[ \t]*$" l-string) (match-string 1 l-string))))
	 l-string
	 )
      (message "!! 2")
      (if l-ccm-addr (setenv "CCM_ADDR" l-ccm-addr) (error "Couldn't find Synergy Command Line Interface, connection failed!"))
      ;; ----------
      ;; retrieve task to work on, this can not be retrieved from the
      ;; developers gui so far (FIXME as soon as fix available from Telelogic)
      ;; ----------
      (message "!! 3")
      (vc-cmsyn-select-task-cli)
      (message "!! 4")
      )
    )
  (vc-cmsyn-sentinel p-process p-what) ;; standard finish
  )

(defun vc-cmsyn-check-status-buffers ()
  "Check/reset status of all buffers in cmsyn mode, refreshes mode-lines.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : "
  (save-excursion
    (dolist (i-buffer (buffer-list))
      (set-buffer i-buffer)
      (when vc-cmsyn-mode
	(vc-cmsyn-check-status-buffer)
	(vc-cmsyn-update-modeline) ;; async process, checks status again afterwards
	)
      )
    )
  )

(defun vc-cmsyn-check-status-buffer ()
  "Checks writable/readonly compared with file-attribs.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (when (buffer-file-name)
    (cond
     ((and (file-writable-p (buffer-file-name)) buffer-read-only)
      (toggle-read-only))
     ((and (not (file-writable-p (buffer-file-name))) (not buffer-read-only))
      (toggle-read-only))
     ))
  )

(defun vc-cmsyn-dos2unix ()
 "Convert this entire buffer from MS-DOS text file format to UNIX."
 (interactive)
 (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "\r$" nil t)
     (replace-match "" nil nil))
   (goto-char (1- (point-max)))
   (if (looking-at "\C-z")
       (delete-char 1))))



;;;###autoload
(defun vc-cmsyn-select-task ()
  "Select a CM Synergy task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-rational-synergy-check-session)
  (if vc-cmsyn-use-developers-gui-p
      (vc-cmsyn-select-task-cli)
    (vc-cmsyn-select-task-gui)
    )
  (vc-rational-synergy-check-session-pause)
  )

(defun vc-cmsyn-select-task-cli ()
  "Select a CM Synergy task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (message "!! vc-cmsyn-select-task-cli")
  (vc-rational-synergy-check-session)
  ;; ----------
  ;; Use the command-interface to retrieve the tasks which will be
  ;; presented 'tabbable' to the user to select 1
  ;; ----------
  (message "  !! vc-cmsyn-select-task-cli2")
  (let* 
      (
      (l-status-string (vc-rational-synergy-command-to-string "status"))
       (l-user (when (string-match "^Sessions for user[ \t]+\\([^:]+\\):" l-status-string) (match-string 1 l-status-string)))
       (l-task-list-string (when l-user (vc-rational-synergy-command-to-string (format "query \"status='task_assigned' and resolver='%s'\" -f \"%%task_number:%%task_description:%%task_synopsis\"" l-user))))
       (l-task-list (mapcar (lambda (p-entry)
			      (let* 
				  (
				   (l-task-line (progn (string-match "^[0-9]+\)[ \t]+\\(.*\\)[ \t]*$" p-entry) (match-string 1 p-entry))) ; chop off 1)
				   (l-task-line (progn (if (string-match "\\([ \t]*\\)$" l-task-line) (replace-match "" t t l-task-line) l-task-line))) ; chop off trailing blanks
				   )
				l-task-line
				)
			      )
			    (split-string l-task-list-string "\n")))
       (l-completion-list (mapcar (lambda (p-entry) (list (progn (string-match "^\\([0-9]+\\):" p-entry) (match-string 1 p-entry)))) l-task-list))
       (minibuffer-help-form (mapconcat 'identity l-task-list "\n"))
       (l-task (completing-read "Select task [tab to complete, help on C-h]: " l-completion-list nil t))
       )
    (message "  !! vc-cmsyn-select-task-cli 3")
    (if (<= (length l-task) 0)
	(message-box "Task not set!")
     (vc-rational-synergy-command-to-string (format "task -default %s" l-task))
      (message-box "Task set to %s!" l-task)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )

(defun vc-cmsyn-select-task-gui ()
  "Select a CM Synergy task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-rational-synergy-check-session)
  (vc-cmsyn-run-command "Calling select Task GUI..." "task -default -g" )
  (when vc-rational-synergy-iconify-frame-when-ccm-gui (iconify-frame))
  (vc-rational-synergy-check-session-pause)
  )

(defun vc-cmsyn-check-task-set (&optional p-signal-error)
  "Ask if a task is running by asking ccm, syncronously
  Author        : Realworld Systems (GR).
  Date          : Apr/2003
  Parameters    : 
  Returns       : boolean"
  (message "Checking ccm task...")
  (let* 
      (
      (l-string (vc-rational-synergy-command-to-string "task -default" ))
       )
    (if (string-match "The default task is not set\\." l-string)
	(if p-signal-error
	    (error "NO default task set!!")
	  nil
	  )
      t
      )
    )
  (message "Checking ccm task...done")
  )

;; Key Bindings
(defvar vc-cmsyn-mode-map
  (make-sparse-keymap)
  "Map for the special keys in minor-mode vc-cmsyn-mode."
  )
(define-key vc-cmsyn-mode-map (kbd "C-c C-m o") 'vc-cmsyn-co-file)
(define-key vc-cmsyn-mode-map (kbd "C-c C-m u") 'vc-cmsyn-undo-co-file)
(define-key vc-cmsyn-mode-map (kbd "C-c C-m i") 'vc-cmsyn-ci-file)
(define-key vc-cmsyn-mode-map (kbd "C-c C-m h") 'vc-cmsyn-history-file-graphics)
(define-key global-map (kbd "C-c C-m d") 'vc-cmsyn-show-default-task)
(define-key global-map (kbd "C-c C-m t") 'vc-cmsyn-ci-task)
(define-key global-map (kbd "C-c C-m s") 'vc-cmsyn-select-task)
(define-key global-map (kbd "C-c C-m p") 'vc-cmsyn-properties)
(define-key global-map (kbd "C-c C-m a") 'vc-cmsyn-about)
(define-key global-map (kbd "C-c C-m l") 'vc-rational-synergy-login) ;; login
(define-key vc-cmsyn-mode-map (kbd "C-c C-m r") 'vc-cmsyn-register-file) ;; register


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun vc-cmsyn-check-set-vc-cmsyn-mode ()
  "Checks if file is in a CM Synergy work-area and sets vc-cmsyn-mode if applicable.
NB: called from within a hook, careful with errors!
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (condition-case l-outer-error
      (progn
	(message (buffer-file-name))
	(let ( (responsible (vc-cmsyn-responsible-p (buffer-file-name)))
	       (mode-on (not vc-cmsyn-mode)) )
	  (when (and responsible mode-on)
	    (condition-case l-error
		(vc-cmsyn-mode) ;; switch on
	      (error (format "Switching %s to CM Synergy mode failed: %s" 
			     (buffer-file-name) 
			     (error-message-string l-error)))))))
    (error (message "Checking for CM Synergy Workarea failed: %s" 
		    (error-message-string 
		     l-outer-error)))))

(define-minor-mode vc-cmsyn-mode
  "Minor-mode for files in a CM Synergy work-area"
  nil " CMSyn" vc-cmsyn-mode-map
  (when vc-cmsyn-mode
    (vc-cmsyn-check-status-buffer) ;; also check here, update modeline goes async
    (vc-cmsyn-update-modeline) ;; also updates status! FIX ME: when into VC this has to be removed!
    )
  )

(defun vc-cmsyn-current-project ()
  "Retrieve the current-project, this only works within a work-area!
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : string or nil"
  (interactive)
  (vc-rational-synergy-check-session)
  (let* 
      (
      (l-string (vc-rational-synergy-command-to-string "status"))
       )
    (save-match-data
      (when (string-match "^Current project:[ \t]+\'\\([^\']+\\)\'" l-string)
	(match-string 1 l-string)
	)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )


(defvar vc-cmsyn-object-name-regexp
  (format "[^%s]+%s\\([^:]+\\):[^:]+:[^:]+" vc-rational-synergy-version-object-separator vc-rational-synergy-version-object-separator)
  "*Regexp to match an object-name of CM Synergy.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  )

(defvar vc-cmsyn-object-name-list-output-regexp
  (format "^[0-9]+\)[ \t]+%s+" vc-cmsyn-object-name-regexp)
;;   (format "^[0-9]+\)[ \t]+[^%s]+%s\\([^:]+\\):[^:]+:[^:]+" vc-rational-synergy-version-object-separator vc-rational-synergy-version-object-separator)
  "*Regexp to match output from ccm that should be a list with objectnames.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  )

(defvar vc-cmsyn-warning-error-output-regexp
  "\\`\\(Warning\\|Error\\):"
  "*Regexp to match waring-error output from CM Synergy.
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  )

(defvar vc-cmsyn-file-created-output-regexp
  "[Mm]ember[ \t]*\\(.*\\)[ \t]*added to project[ \t]+\\([^ \t\n]+\\)[ \t]*$"
  "*Regexp to match correct file-creation output from CM Synergy.
  subexp 1 is fileleafname<`vc-rational-synergy-version-object-separator'>version
  subexp 2 is project-name
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  )

(defvar vc-cmsyn-file-ci-output-regexp
  "Checked in[ \t]*'\\([^']+\\)'[ \t]*to[ \t]*'\\([^']+\\)'"
  "*Regexp to match correct file-ci output from CM Synergy.
  subexp 1 is fileleafname<`vc-rational-synergy-version-object-separator'>version
  subexp 2 is new status
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  )

;; ----------
;; EDIFF for CMSynergy
;; ----------
;;;###autoload
(defun vc-cmsyn-ediff-versions ()
  "   This asks for versions to compare, then shows both versions of current file, using ediff.
                  (unlike vc-version-diff)
  Date          : Mar/2001
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld OO Systems B.V. (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-v1            (read-string "Older version: "))
	 (l-v2            (read-string "Newer version: "))
	 (l-object-name   (vc-rational-synergy-command-to-string (format "ls -f \"%%objectname\" %s" (vc-rational-synergy-platformify-path l-file))))
	 l-low-buffer l-high-buffer l-string1 l-string2 l-file1 l-file2
	 )
      (if (not (string-match vc-cmsyn-object-name-regexp l-object-name))
	  (error "Couldnt retrieve the right file-information from CMSynergy: %s" l-object-name)
	(save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
	(setq l-file1 (replace-match l-v1 nil nil l-object-name 1))
	(setq l-file2 (replace-match l-v2 nil nil l-object-name 1))
	(setq l-string1 (vc-rational-synergy-command-to-string (format "cat %s" l-file1)))
	(when (string-match vc-cmsyn-warning-error-output-regexp l-string1)
	  (error (format "CM Synergy Error: %s" l-string1)))
	(setq l-string2 (vc-rational-synergy-command-to-string (format "cat %s" l-file2)))
	(when (string-match vc-cmsyn-warning-error-output-regexp l-string2)
	  (error (format "CM Synergy Error: %s" l-string2)))
	(save-excursion
	  (setq l-low-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-v1))))
	  (erase-buffer)
	  (insert l-string1)
	  (when (equal system-type 'windows-nt)
	    (goto-char (point-min))
	    (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	    (goto-char (point-max))
	    (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	    )
	  )
	(save-excursion
	  (setq l-high-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-v2))))
	  (insert l-string2)
	  (when (equal system-type 'windows-nt)
	    (goto-char (point-min))
	    (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	    (goto-char (point-max))
	    (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	    )
	  )
	(ediff-buffers l-low-buffer l-high-buffer)
	)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-ediff-other-version ()
  "   This asks for versions to compare, then shows both versions of current file, using ediff.
                  (unlike vc-version-diff)
  Date          : Mar/2001
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld OO Systems B.V. (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when  (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-v1            (read-string "Other version: "))
	 (l-object-name   (vc-rational-synergy-command-to-string (format "ls -f \"%%objectname\" %s" (vc-rational-synergy-platformify-path l-file))))
	 (l-high-buffer    (current-buffer))
	 l-low-buffer l-string1 l-string2 l-file1 l-file2
	 )
      (if (not (string-match vc-cmsyn-object-name-regexp l-object-name))
	  (error "Couldnt retrieve the right file-information from CMSynergy: %s" l-object-name)
	(save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
	(setq l-file1 (replace-match l-v1 nil nil l-object-name 1))
	(setq l-string1 (vc-rational-synergy-command-to-string (format "cat %s" l-file1)))
	(when (string-match vc-cmsyn-warning-error-output-regexp l-string1)
	  (error (format "CM Synergy Error: %s" l-string1)))
	(save-excursion
	  (setq l-low-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-v1))))
	  (erase-buffer)
	  (insert l-string1)
	  (when (equal system-type 'windows-nt)
	    (goto-char (point-min))
	    (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	    (goto-char (point-max))
	    (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	    )
	  )
	(ediff-buffers l-low-buffer l-high-buffer)
	)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-ediff ()
  "   Compare the current version with the parent version, using ediff.
  Date          : Mar/2001
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld OO Systems B.V. (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when  (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-object-name   (vc-rational-synergy-command-to-string (format "ls -f \"%%objectname\" %s" (vc-rational-synergy-platformify-path l-file))))
	 (l-high-buffer   (current-buffer))
	 l-parent-object-name l-low-buffer l-parent-object-name-string l-parent-object-name l-string1 l-lines
	 )
      (when (not (string-match vc-cmsyn-object-name-regexp l-object-name))
	(error "Couldnt retrieve the right file-information from CMSynergy for object: %s" l-object-name)
	)
      (save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
      (setq l-parent-object-name (vc-cmsyn-parent-4-part-name l-object-name))
      (cond ((not l-parent-object-name)
	     (error "No ancestor found of %s" l-file))
	    ((listp l-parent-object-name)
	     (setq l-parent-object-name (completing-read "More than 1 ancestor, choose which 1 to compare [tab to complete]: " l-parent-object-name))
	     )
	    )
     (setq l-string1 (vc-rational-synergy-command-to-string (format "cat %s" l-parent-object-name)))
      (when (string-match vc-cmsyn-warning-error-output-regexp l-string1)
	      (error (format "CM Synergy Error: %s" l-string1)))
      (save-excursion
	(setq l-low-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-parent-object-name))))
	(erase-buffer)
	(insert l-string1)
	(when (equal system-type 'windows-nt)
	  (goto-char (point-min))
	  (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	  (goto-char (point-max))
	  (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	  )
	)
      (ediff-buffers l-low-buffer l-high-buffer)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )

(defun vc-cmsyn-parent-4-part-name (p-4-part-name)
  "Retrieve the parent-4-part-name of file in current-buffer.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : 
  Returns       : String or list with strings (if more than 1 ancessor)"
  (let* 
      (
      (l-parent-object-name-string (vc-rational-synergy-command-to-string (format "query \"is_predecessor_of ('%s')\" -f \"%%objectname\"" p-4-part-name)))
       (l-lines (split-string l-parent-object-name-string "\n"))
       )
    (if (equal (length l-lines) 1)
	(nth 1 (split-string (car l-lines)))
      (let ((l-list (mapcar (lambda (p-line)
			      (nth 1 p-line) ;; format is <nr>) <objectname>
			      )
			    l-lines)))
	l-list
	)
      )
    )
  )

(defun vc-cmsyn-wa-path-for (p-project-name-version)
  "Retrieves the workarea-path for project P-PROJECT-NAME-VERSION.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : P-PROJECT-NAME-VERSION \"<project-name><object-version-separator><version>\"
  Returns       : path or nil"
  (let* 
      (
      (l-wa-string	 (vc-rational-synergy-command-to-string (format "attr -show wa_path -project %s" p-project-name-version))) ;; workarea output
       (l-wa-path	 (save-match-data (string-match "^\\(.*\\)[ \t]*\n?" l-wa-string) (match-string 1 l-wa-string))) ;; strip off \n
       )
    
    (when (and (not (string-match "\\(Project reference requires name and version:\\|Specified project cannot be identified:\\)" l-wa-string))
	       (not (string-match vc-cmsyn-warning-error-output-regexp l-wa-string)))
      l-wa-path
      )
    )
  )

(defun vc-cmsyn-object-name-2-path (p-4-part-name &optional p-no-error-p p-prompt-project-if-not-unique p-project-name)
  "Return the file-path for P-4-PART-NAME.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : P-4-PART-NAME: Unique CM Synergy object-name
		  P-NO-ERROR-P: if non-nil no error will be generated when file is in more than 1 project
		  P-PROMPT-PROJECT-IF-NOT-UNIQUE: if non-nil, user will be prompted to choose a project when file in more than 1 project.
		  P-PROJECT-NAME: if supplied and file is in more than 1 project
  Returns       : list with file-path and current-project or nil"
  ;; ----------
  ;; methodology: * get the project this object belongs to (finduse, if in more than 1,
  ;;		    filter on status, if not P-NO-ERROR-P error, prompt
  ;;		    if P-PROMPT-PROJECT-IF-NOT-UNIQUE)
  ;;              * get the project-path of the project 
  ;;              * get the relative path within the project (shown
  ;;		    within the finduse string)
  ;;		  * combine the absolute and relative path
  ;; ----------
  (let* 
      (
      (l-ccm-string (vc-rational-synergy-command-to-string (format "finduse %s" p-4-part-name)))
       l-path l-rel-path l-rel-path-version l-project-path l-project l-projects-alist l-start
       )
    (if (string-match "@[^@]*@" l-ccm-string)
	(progn
	  ;; ----------
	  ;; multiple projects, derive 1 project 1st!!
	  ;; ----------
	  (setq l-projects-alist nil)
	  (setq l-start 0)
	  (while (string-match "@\\(.*\\)$" l-ccm-string l-start)
	    (push (list (match-string 1 l-ccm-string)) l-projects-alist)
	    (setq l-start (match-end 0))
	    )
	  ;; ----------
	  ;; filter projects in working|visible|shared status
	  ;; Leave out for the moment: not sure which statuses should be
	  ;; filtered out
	  ;; ----------
	  (cond
	   ((equal (length l-projects-alist) 1)
	    ;; left exactly 1
	    (setq l-project (caar l-projects-alist))
	    )
	   ((> (length l-projects-alist) 1)
	    ;; left more than 1, handle parameter-project, prompt or error
	    (cond
	     ((and (not p-no-error-p) (not p-prompt-project-if-not-unique) (not p-project-name))
	      (error "%s is used in more than 1 project, can't determine a unique Path for it!" p-4-part-name)
	      )
	     (p-project-name ;; more than 1 but cached project-to-choose supplied
	      (setq l-project ;; check if p-project-name is in the project-list
		    (loop for i-list in l-projects-alist
			  do
			  (when (equal p-project-name (car i-list))
			    (return p-project-name)
			    )
			  ))
	      (when (not l-project)
		(message "%s is in multiple-projects, not 1 matching cached project %s!" p-4-part-name p-project-name)
		(if p-no-error-p
		    (message-box "%s is in multiple-projects, not 1 matching cached project %s!" p-4-part-name p-project-name)
		  (error "%s is in multiple-projects, not 1 matching cached project %s!" p-4-part-name p-project-name)
		  )
		)
	      )
	     (p-prompt-project-if-not-unique ;; no cached project supplied, prompt for a project
	      (message "File %s used in more than 1 project!" p-4-part-name)
	      (setq l-project (completing-read (format "File %s used in more than 1 project, choose project [tab to complete]" p-4-part-name) l-projects-alist nil t))
	      (string-match (format "^[ \t]*\\(.*\\)@%s" l-project) l-ccm-string)
	      (setq l-rel-path-version (match-string 1 l-ccm-string))
	      )
	     (t ;; no project found
	      (message "No project found for file %s!" p-4-part-name)
	      (if p-no-error-p
		  (message-box "%s not used in a project, can't determine a unique Path for it!" p-4-part-name)
		(error "%s not used in a project, can't determine a unique Path for it!" p-4-part-name)
		)
	      )
	     )
	    )
	   (t ;; no project found,
	    (message "No project found for file %s!" p-4-part-name)
	    (if p-no-error-p
		(message-box "%s not used in a project, can't determine a unique Path for it!" p-4-part-name)
	      (error "%s not used in a project, can't determine a unique Path for it!" p-4-part-name)
	      )
	    )
	   )
	  )
      ;;-----------
      ;; single project (if any)
      ;; ----------
      (if (not (and (string-match "^[ \t]*\\([^\n@]+\\)@\\([^@\n]+\\)$" l-ccm-string)
		    (not (save-match-data (string-match vc-cmsyn-warning-error-output-regexp l-ccm-string)))
		    ))
	  (progn
	    (message "%s not used in a project, can't determine a unique Path for it!" p-4-part-name)
	    (if p-no-error-p
		(message-box "%s not used in a project, can't determine a unique Path for it!" p-4-part-name)
	      (error "%s not used in a project, can't determine a unique Path for it!" p-4-part-name)))
	(setq l-project (match-string 2 l-ccm-string))
	(setq l-rel-path-version (match-string 1 l-ccm-string))
	)
      )
    (setq l-project-path    (when l-project (vc-cmsyn-wa-path-for l-project)))
    ;; trim white-space
    (setq l-project-path    (when l-project-path (save-match-data (string-match "[ \t]*\\([^ \t]+\\)[ \t]*" l-project-path) (match-string 1 l-project-path))))
    (setq l-rel-path	    (when l-rel-path-version (car (split-string l-rel-path-version vc-rational-synergy-version-object-separator))))
    ;; trim white-space
    (setq l-rel-path	    (when l-rel-path (save-match-data (string-match "[ \t]*\\([^ \t]+\\)[ \t]*" l-rel-path) (match-string 1 l-rel-path))))
    (when l-rel-path	    (list (expand-file-name l-rel-path l-project-path) l-project)) ;; result
    )
  )

(defun vc-cmsyn-name-from-4-part-name (p-4-part-name)
  "Deduct the file-leaf-name of the object from P-4-PART-NAME
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : P-4-PART-NAME: the CM Synergy unique 4-part-name.
  Returns       : string"
  (save-match-data (string-match (format "^\\([^%s]+\\)%s" vc-rational-synergy-version-object-separator vc-rational-synergy-version-object-separator) p-4-part-name) (match-string 1 p-4-part-name))
  )

(defun vc-cmsyn-base-line-4-part-name-of (p-4-part-name)
  "Retrieve the 4-part-name of the base-line-version of P-4-PART-NAME.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : P-4-PART-NAME: the 4-part-name that we want the base-line-version of
  Returns       : 4-part-name-string or nil"
  (let* 
      (
       (l-list		      (split-string p-4-part-name (format "[%s:]" vc-rational-synergy-version-object-separator)))
       (l-ccm-type	      (nth 2 l-list))
       (l-instance	      (nth 3 l-list))
       (l-filename	      (nth 0 l-list))
       (l-use		      (vc-rational-synergy-command-to-string (format "finduse %s" p-4-part-name)))
       l-project l-rp-bl-string l-bl-project l-file-1 l-result l-list
       )
    (if (not (string-match "@" l-use))
	(setq l-result nil) ;; not used, or error-message, forget about it
      (setq l-project (car (split-string (nth 1 (split-string l-use "@")) "\n")))
      (setq l-rp-bl-string (vc-rational-synergy-command-to-string
			    (format "rp -show bl %s" l-project) ;; format <nr>) <projectname>\n
			    ))
      (if (not (string-match "^[0-9]+\)[ \t]+[^ \t]+[ \t]*\n" l-rp-bl-string))
	  (setq l-result nil) ;; not the right output, forget about this? Message?
	(setq l-bl-project (nth 1 (split-string l-rp-bl-string)))
	(setq l-file-1 (vc-rational-synergy-command-to-string
			(format "query \"is_member_of('%s') and name='%s' and type='%s' and instance='%s' \" -f \"%%objectname\""
				l-bl-project l-filename l-ccm-type l-instance)
			)
	      ) ;; base-line-version
	)
      (if (and (not (zerop (length l-file-1)))
	       (> (length (setq l-list (split-string l-file-1 ))) 1)
	       )
	  (setq l-result (nth 1 l-list))
	(setq l-result nil)
	)
      )
    l-result
    )
  )


(defun vc-cmsyn-responsible-p (p-file &optional p-prev p-absolute)
 "Check if the file P-FILE is located in a CM Synergy workarea"
 (let* ( (l-file     (if p-absolute
			 p-file
		       (vc-rational-synergy-unixify-path p-file))       )   ;; unixify
	 (l-path     (file-name-directory l-file)            )
	 (l-expanded (expand-file-name "_ccmwaid.inf" l-path))
	 (l-up       (expand-file-name ".." l-path)          )
	 (l-up-trail (if (string-match "/$" l-up)
			 l-up
		       (concat l-up "/"))                    )
	 (l-prev     (if p-prev p-prev "")                   ) )
   

   (cond ((file-exists-p l-expanded)
	  l-expanded)
	 ((string= l-prev l-expanded)
	  '())
	 (t
	  (vc-cmsyn-responsible-p l-up-trail l-expanded t)))))

(defun vc-cmsyn-could-register (p-file)
 "Return non-nil if P-FILE could be registered in CMSyn.
This is only possible if CMSyn is responsible for P-FILE's directory."
 (vc-cmsyn-responsible-p p-file))

(defun vc-cmsyn-registered (p-file)
 "Return non-nil if FILE is registered in CM Synergy, NB: calls ccm, *slow*.
 Author        : Realworld Systems (GR)
 Date          : Apr/2003
 Parameters    : P-FILE: file-path to check
 Returns       : nil or workfile-version"
 (vc-cmsyn-workfile-version p-file)
 )

(defun vc-cmsyn-workfile-version (p-file)
 "CM Synergy version of `vc-workfile-version', this is *slow* when file is in CM Synergy!.
 Author        : Realworld Systems (GR)
 Date          : Apr/2003
 Parameters    : P-FILE: File we want the version of
 Returns       : version (string) or nil"
 (let* 
     (
      (l-responsible (vc-cmsyn-responsible-p p-file)) ;; fast
      )
   (when l-responsible ;; in CM Synergy workarea
     (or (vc-file-getprop p-file 'vc-workfile-version)
	 ;; ----------
	 ;; now it gets slow, it wasn't cached yet
	 ;; ----------
	 (let* 
	     (
	      (l-file			(buffer-file-name))
	      (l-object-name		(vc-rational-synergy-command-to-string (format "ls -f \"%%objectname\" %s" (vc-rational-synergy-platformify-path l-file))))
	      l-version
	      )
	   (if (not (string-match vc-cmsyn-object-name-regexp l-object-name))
	       (error "Couldn't determine CM Synergy object-name of %s" l-file)
	     (setq l-version (match-string 1 l-object-name))
	     (vc-file-setprop l-file 'vc-workfile-version l-version)
	     l-version
	     )
	   )
	 )
     )
   )
 )

(provide 'vc-rational-synergy-base)

;;; vc-rational-synergy-base.el ends here
