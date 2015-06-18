;;; vc-rational-synergy-sentinels.el --- IBM Rational Synergy sentinels

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

;; These functions are sentinel functions for all IBM Rational Synergy
;; process requests running in a buffer

;; Code:

(require 'vc-rational-synergy-utilities)

(defun vc-rational-synergy-sentinel (process what &optional hidden)
  "Function running at the end of a ccm PROCESS called directly as sentinel.
Could also be called from specific sentinels for specific funtions.
WHAT registers the change in a given ccm PROCESS and setting HIDDEN to non-nil
hides the ccm output-buffer."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       )
    (set-buffer output-buffer)
    (goto-char (point-max))
    (vc-rational-synergy-dos2unix)
    ;; ----------
    ;; and clear the buffer-proc-mapping
    ;; ----------
    (vc-cmsyn-clear-buffer-mapping-for-proc process)
    (vc-rational-synergy-report (format "CM Synergy Command %s" what))
    (when (and vc-rational-synergy-command-end-jump-to-output-buffer-p
	       (not hidden)
	       )
      (vc-cmsyn-show-output-buffer)
      )
    (set-buffer buffer)
    (vc-rational-synergy-update-modeline) ;; and this also sets status-buffer!
    )
  )


(defun vc-rational-synergy-sentinel-co-file (process what &optional dont-check-parallel-versions)
  "Function running at the end of the ccm co process for a file, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
		  DONT-CHECK-PARALLEL-VERSIONS: if non-nil the parallel-version-check is skipped: used when this sentinel is
		    called from another, instead of directly as sentinel.
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       )
    ;; ----------
    ;; check parallel versions
    ;; ----------
    (when (not dont-check-parallel-versions)
      (save-excursion
	(set-buffer output-buffer)
	(when (re-search-backward "Starting check out of" nil t)
	  (when (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	    (x-popup-dialog t (list (match-string 0) (cons "Ok" t)))
	    )
	  )
	)
      )
    (set-buffer buffer)
    (when (string-match "finished" what) ;; carefull with blanks and newlines in p_what!
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
	     (filename (vc-rational-synergy-platformify-path (buffer-file-name)))
	     (dir (file-name-directory filename))
	     (proc (start-process
		      "vc-cmsyn"
		      " *SYNERGY-AUTO-CHECK-IN*"
		      vc-rational-synergy-binary-name
		      (format "ci -c \"Checked in automatically after co of %s by emacs/vc-cmsyn\" %s" filename dir)
		      )
		     )
	     )
	  )
	)
      )
    (vc-rational-synergy-sentinel process what) ;; cleanup, evt. remove C-m's
    (message "Ready checking out file %s" (buffer-file-name buffer))
    )
  )

(defun vc-rational-synergy-sentinel-register-file (process what)
  "Function running at the end of the ccm co process for a file, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       )
    (set-buffer buffer)
    (when (equal what "finished")
      (find-file (buffer-name buffer)) ;; get the version that was checked out from CM Synergy
      )
    ;; ----------
    ;; And ci this file now that it's registered
    ;; ----------
    (if vc-rational-synergy-register-checks-in-p
	(vc-cmsyn-ci-file "1st Checkin after creation") ;; this also pops to output-buffer and checks status buffer
      ;;       (vc-cmsyn-check-status-buffer) ==> via update-mode-line in (vc-rational-synergy-sentinel)
      (vc-rational-synergy-sentinel process what) ;; cleanup, evt. remove C-m's
      (message "%s is registered, but not checked in yet!" (buffer-file-name))
      )
    (message "Ready registering %s" (buffer-file-name buffer))
    )
  )

(defun vc-rational-synergy-sentinel-co-directory (process what)
  "Function running at the end of the ccm co process for a directory, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       dir message-string
       )
    (set-buffer buffer)
    ;; ----------
    ;; check parallel versions for 1 of the files
    ;; ----------
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-max))
      (while (re-search-backward "Starting check out " vc-rational-synergy-process-start-mark t))
      (while (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	(setq message-string (format "%s; %s" message-string (match-string 0)))
	)
      (when (> (length message-string) 0) (x-popup-dialog t (list message-string (cons "Ok" t))))
      )
    (if (buffer-file-name)
	(progn
	  (setq dir (file-name-directory (buffer-file-name)))
	  (vc-rational-synergy-sentinel-co-file process what t)) ;; don't check parallel versions
      (setq dir (if (consp dired-directory) (car dired-directory) dired-directory))
      (revert-buffer) ;; just refresh, this is a directory
      )
    (vc-rational-synergy-sentinel process what) ;; cleanup, evt. remove C-m's, mode-line refresh
    (message "Ready checking out directory %s" dir)
    )
  )

(defun vc-rational-synergy-sentinel-ci-file (process what)
  "Function running at the end of the ccm ci process for a file, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       )
    ;; ----------
    ;; check parallel versions
    ;; ----------
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-max))
      (when (re-search-backward "Starting check in of" vc-rational-synergy-process-start-mark t)
	(when (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	  (x-popup-dialog t (list (match-string 0) (cons "Ok" t)))
	  )
	)
      )
    (set-buffer buffer)
    (revert-buffer t t)
    (vc-rational-synergy-sentinel process what)
    (message "Ready checking in file %s" (buffer-file-name buffer))
    )
  )

(defun vc-rational-synergy-sentinel-undo-co-file (process what)
  "Function running at the end of the ccm ci process for a file, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       )
    (set-buffer buffer)
    (revert-buffer t t) ;; Synergy has rolled back the file, make visible
    (vc-rational-synergy-sentinel process what)
    (message "Ready undoing check out of  %s" (buffer-file-name buffer))
    )
  )

(defun vc-rational-synergy-sentinel-ci-directory (process what)
  "Function running at the end of the ccm ci process for a directory, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       (message-string "")
       )
    ;; ----------
    ;; check parallel versions for 1 of the files
    ;; ----------
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-max))
      (while (re-search-backward "Starting check in " vc-rational-synergy-process-start-mark t))
      (while (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	(setq message-string (format "%s; %s" message-string (match-string 0)))
	)
      (when (> (length message-string) 0) (x-popup-dialog t (list message-string (cons "Ok" t))))
      )
    (save-excursion
      (set-buffer buffer)
      (when (not (buffer-file-name))
	(revert-buffer) ;; just refresh, this is a directory
	)
      )
    (vc-cmsyn-check-status-buffers)
    (vc-cmsyn-clear-buffer-mapping-for-proc process)
    (message "Ready checking in directory")
    )
  )

(defun vc-rational-synergy-sentinel-undo-co-directory (process what)
  "Function running at the end of the ccm undo co process for a directory, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       revert-buffer
       )
    ;; ----------
    ;; find the revert-buffer, either the directory of the buffer where
    ;; the command was called, or the buffer itself
    ;; ----------
    (save-excursion 
      (set-buffer buffer)
      (if (not (buffer-file-name))
	  (setq revert-buffer buffer ) ;; just refresh, this is a directory
	(setq revert-buffer (find-buffer-visiting (file-name-directory (buffer-file-name))))
	)
      (when revert-buffer
	(save-excursion
	  (set-buffer revert-buffer)
	  (revert-buffer t t)
	  )
	)
      )
    (vc-cmsyn-clear-buffer-mapping-for-proc process)
    (message "Ready undoing check out of directory")
    )
  )

(defun vc-rational-synergy-sentinel-ci-task (process what) 
  "Function running at the end of the ccm ci process for a task, called directly as sentinel, calls the general `vc-rational-synergy-sentinel'.
  Date          : Apr/2003
  Parameters    : PROCESS: the ccm process 
                  WHAT: the change in the ccm process
  Returns       : 
  Methodology   : 
  Author        : Realworld Systems (GR)."
  (let* 
      (
       (buffer (vc-cmsyn-get-buffer-mapped-to-proc process))
       (output-buffer (process-buffer process))
       (message-string "")
       )
    ;; ----------
    ;; check parallel versions for 1 of the files
    ;; ----------
    (save-excursion
      (set-buffer output-buffer)
      (goto-char (point-max))
      (while (re-search-backward "Starting check in " vc-rational-synergy-process-start-mark t))
      (while (re-search-forward (format "%s.*$" vc-rational-synergy-parallel-versions-string ) nil t)
	(setq message-string (format "%s; %s" message-string (match-string 0)))
	)
      (when (> (length message-string) 0) (x-popup-dialog t (list message-string (cons "Ok" t))))
      )
    (save-excursion
      (set-buffer buffer)
      (when (not (buffer-file-name))
	(revert-buffer) ;; just refresh, this is a directory
	)
      )
    ;; ----------
    ;; now update status of all vc-cmsyn-buffers!
    ;; ----------
    (vc-cmsyn-check-status-buffers)
    (vc-cmsyn-clear-buffer-mapping-for-proc process)
    (message "Ready checking in task")
    )
  )

(defun vc-rational-synergy-sentinel-history-graphics (process what)
  "Function running when the ccm process for displaying graphical history information is done.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : PROCESS: ccm process
                  WHAT: change in process
  Returns       : "
  (vc-rational-synergy-sentinel process what t) ;; standard finish, but don't popup the process-buffer, would cover the graphics-history-output
  )

(defun vc-rational-synergy-sentinel-start-developers-gui (process what)
  "Function running when the ccm login process is done.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : PROCESS: ccm process
                  WHAT: change in process
  Returns       : "
  (message "!! vc-rational-synergy-sentinel-start-developers-gui 1")
  (save-excursion
    (let* 
	(
	 (message-string (vc-rational-synergy-command-to-string "status")) ;; find out ccm_addr cli
	 (ccm-addr (progn 
		     (when (string-match  "^Command[ \t]+Interface[ \t]+@ \\(.*\\)[ \t]*\\((current session)\\)?[ \t]*$" message-string) 
		       (match-string 1 message-string))))
	 )
      (if ccm-addr (setenv "CCM_ADDR" ccm-addr) (error "Couldn't find Synergy Command Line Interface, connection failed!"))
      ;; ----------
      ;; retrieve task to work on, this can not be retrieved from the
      ;; developers gui so far (FIXME as soon as fix available from Telelogic)
      ;; ----------
      (vc-cmsyn-select-task-cli)
      )
    )
  (vc-rational-synergy-sentinel process what) ;; standard finish
  )


(provide 'vc-rational-synergy-sentinels)

;; vc-rational-synergy-sentinels.el ends here
