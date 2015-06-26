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


(defun vc-rational-synergy--synergized-buffers ()
  "Returns all buffers part of the default task." 
    (let* ((files (vc-rational-synergy--working-task-files)))
      (delq nil
	    (mapcar (lambda (b)
		      (with-current-buffer b
			(when (and (buffer-file-name)
				   (member (buffer-file-name) files))
			  b)))
		    (buffer-list)))))

(defun vc-rational-synergy--revert-working-buffers ()
  "Checks if the default task associates unsaved buffers."
  (mapcar (lambda (b) (revert-buffer nil t))
	  (vc-rational-synergy--synergized-buffers)))


(defun vc-rational-synergy--buffers-modified-p ()
  "Checks if the default task associates unsaved buffers."
  (catch 'exit
    (let* ((synergized (vc-rational-synergy--synergized-buffers))
      (dolist (buffer synergized)
	(with-current-buffer buffer
	  (when (buffer-modified-p) (throw 'exit t))))))))


(defun vc-rational-synergy-buffer ()
  "Return the output-buffer for the ccm process.
If the buffer does not exist yet, create it within the current frame"
  (interactive)
  
  (save-excursion
    (selected-frame)
    (get-buffer-create vc-rational-synergy-buffer-name)))

(defun vc-rational-synergy-buffer--wind-to-end ()
  "Winds the synergy buffer to the end
Sets cursor at point-max"

  (save-excursion
    (set-buffer (vc-rational-synergy-buffer))
    (goto-char (point-max))
    (setq vc-rational-synergy-process-start-mark (point-marker))))



(defun vc-cmsyn-buffer-insert (p-message)
  "Insert message P-MESSAGE.in the vc-cmsyn output buffer
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-MESSAGE: message to insert
  Returns       : "
  (with-current-buffer (vc-rational-synergy-buffer)
    (goto-char (point-max))
    (insert (format "\n%s\n" p-message))
    )
  )

(defun vc-rational-synergy-report (p-message)
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

(defun vc-cmsyn-show-output-buffer ()
  "Switch to the output-buffer and put cursor at the end, this may be to the separate frame or within the current frame.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (let* 
      (
       (l-proc-buffer (vc-rational-synergy-buffer))
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
	(vc-rational-synergy-update-modeline) ;; async process, checks status again afterwards
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

(provide 'vc-rational-synergy-buffer)

;; vc-rational-synergy-buffer.el ends here
