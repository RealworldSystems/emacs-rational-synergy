;;; vc-cmsyn-el --- IBM Rational Synergy integration for Emacs
 
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


;; ----------
;; Utilities
;; ----------
;;;###autoload
(defun vc-cmsyn-command-to-string (p-command-string)
  "Call `vc-cmsyn-exe-name' with argument-string P-COMMAND-STRING.
command-string is passed to ccm via ascii-file to avoid quoting-problems.
  Author        : Realworld Systems (GR)
  Date          : Oct/2004
  Parameters    : P-COMMAND-ARGS: list of arguments to the `vc-cmsyn-exe-name' command
  Returns       : output of the command as string"
  (save-excursion
    (when (zerop (length p-command-string)) (error "vc-cmsyn-command-to-string called without argument-string"))
    ;; ----------
    ;; write string to file 1st to prevend complicated quotation problems,
    ;; do not use command-shell coz sometimes processes keep hanging
    ;; ----------
    (let* 
	(
	 (l-tmp-file-name (expand-file-name "tmp_syn.syn" (getenv "TEMP")))
	 (l-buffer (get-buffer-create "*vc-cmsyn_tmp*"))
	 )
      (set-buffer l-buffer)
      (erase-buffer)
      (insert p-command-string)
      (write-file l-tmp-file-name)
      (kill-this-buffer)
      (with-output-to-string
	(with-current-buffer
	    standard-output
	  (call-process vc-cmsyn-exe-name nil t nil "source" (vc-cmsyn-platformity-path l-tmp-file-name))
	  )
	)
;;;     (kill-buffer l-buffer)
      )
    )
  )

;;;###autoload
(defun vc-cmsyn-unixify-path (p-path)
"Converts unix-style path to windows and vice-versa (won't add drive).
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PATH: path to convert
  Returns       : "
(mapconcat '(lambda(x) (char-to-string (if (eq x ?\\) ?/ x))) p-path nil)
)

;;;###autoload
(defun vc-cmsyn-platformity-path (p-path)
  "Converts unix-style path to windows and vice-versa (won't add drive).
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PATH: path to convert
  Returns       : "
  (if (eq system-type 'windows-nt)
      (mapconcat '(lambda(x) (char-to-string (if (eq x ?/) ?\\ x))) p-path nil)
    (mapconcat '(lambda(x) (char-to-string (if (eq x ?\\) ?/ x))) p-path nil)
    )
  )

(if (not (fboundp `redraw-modeline))
    (defalias `redraw-modeline `force-mode-line-update))

(defvar vc-cmsyn-modeline-string nil
"CMSyn information, displayed in the modeline.")
(make-variable-buffer-local 'vc-cmsyn-modeline-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Commands

(defun vc-cmsyn-check-login ()
  "Check if login is necessary and if so. do login with stored information.
  Author        : Realworld Systems (GR)
  Date          : Mar/2004
  Parameters    : 
  Returns       : non-nil if login was successfull"
;;;   (message "!! vc-cmsyn-check-login" )
  (let* 
      (
       (l-ccm_addr (getenv "CCM_ADDR"))
;;;        (l-time (string-to-number (format-time-string "%S")))
       l-ccm-string l-new-ccm-addr l-user l-pw l-db l-task-string
       )
    (when vc-cmsyn-auto-login-logout
;;;       (message "!! REAL AUTO login")
      (when (zerop (length vc-cmsyn-auto-login-task))
	(customize-variable 'vc-cmsyn-auto-login-task)
	(message-box "Auto-login Task not set, needs task to be set to work with auto-login to Synergy, aborting!")
	(error "Auto-login Task not set, needs task to be set to work with auto-login to Synergy!")
	)
      (setq l-user	 (nth 0 vc-cmsyn-auto-login-data))
      (setq l-pw	 (nth 1 vc-cmsyn-auto-login-data))
      (setq l-db	 (nth 2 vc-cmsyn-auto-login-data))
      (when (zerop (length l-user))
	(customize-variable 'vc-cmsyn-auto-login-data)
	(message-box "Auto-login Task not set, needs task to be set to work with auto-login to Synergy, aborting")
	(error "Auto-login Task not set, needs task to be set to work with auto-login to Synergy!")
	)
      (message "Autologin to CM Synergy...")
;;;       (message "Starting ccm-session with command: -%s-" (format "%s start -m -pw %s -n %s -d %s -nogui" vc-cmsyn-exe-name l-pw l-user l-db))
;;;       (setq l-ccm-string (vc-cmsyn-command-to-string "start" "-m" "-pw" l-pw "-n" l-user "-d" l-db " -nogui"))
      (setq l-ccm-string (vc-cmsyn-command-to-string (format "start -m -pw %s -n %s -d %s -nogui" l-pw l-user l-db)))
;;;       (message "!! Analyzing string: -%s-" l-ccm-string)
      (cond ;; difference in 1st session and multiple-sessions
       ((string-match  "^Command[ \t]+Interface[ \t]+@ \\(.*\\)[ \t]*\\((current session)\\)?[ \t]*$" l-ccm-string)
	(setq l-new-ccm-addr (match-string 1 l-ccm-string))l-db
	)
       ((string-match "CM Synergy address is[ \t]*\\(.*\\)$" l-ccm-string)
	(setq l-new-ccm-addr (match-string 1 l-ccm-string))
	)
       )
;;;      (if (string-match  "^Command[ \t]+Interface[ \t]+@ \\(.*\\)[ \t]*\\((current session)\\)?[ \t]*$" l-ccm-string)
;;;	  (setq l-new-ccm-addr (match-string 1 l-ccm-string))
;;;	(message "Dcheking for multiple-session, match: -%s-" (string-match "CM Synergy address is[ \t]*\\(.*\\)$" l-ccm-string))
;;;	(when (string-match "CM Synergy address is[ \t]*\\(.*\\)$" l-ccm-string)
;;;	  (setq l-new-ccm-addr (match-string 1 l-ccm-string))
;;;	  (message "!! new-ccm-addr2: -%s-" l-new-ccm-addr)
;;;	  )
;;;	)
;;;       (setq l-new-ccm-addr (progn (when (string-match  "^Command[ \t]+Interface[ \t]+@ \\(.*\\)[ \t]*\\((current session)\\)?[ \t]*$" l-ccm-string) (match-string 1 l-string))))
      (setq l-new-ccm-addr (if (and l-new-ccm-addr (string-match "[ \t]*(current session)[ \t]*" l-new-ccm-addr)) (replace-match "" t t l-new-ccm-addr) l-new-ccm-addr))
      (setq l-new-ccm-addr (if (and l-new-ccm-addr (string-match "\\([ \t]*\\.[ \t]*$\\)" l-new-ccm-addr)) (replace-match "" t t l-new-ccm-addr 1) l-new-ccm-addr))
;;;       (message "Started Address: -%s-, current env: -%s-" l-new-ccm-addr (getenv "ccm_addr"))
      (setenv "CCM_ADDR" l-new-ccm-addr) ;; set this 1 current, stop should only quit this 1!
;;;       (message "!! env set to: -%s-, check-login took: -%s-" (getenv "CCM_ADDR") (- (string-to-number (format-time-string "%S")) l-time))
;;;       (setq l-task-string (vc-cmsyn-command-to-string "task" "-default" vc-cmsyn-auto-login-task))
      (setq l-task-string (vc-cmsyn-command-to-string (format "task -default %s" vc-cmsyn-auto-login-task)))
;;;       (message "!! Task-output: -%s-" l-task-string)
      (save-match-data (string-match "CM Synergy server starting on host" l-ccm-string)) ;; if successfully
      (message "Autologin to CM Synergy...done")
      )
    )
  )

(defun vc-cmsyn-check-logout ()
  "Check if login is necessary and if so. do login with stored information.
  Author        : Realworld Systems (GR)
  Date          : Mar/2004
  Parameters    : 
  Returns       : non-nil if logout was successfull"
;;;   (message "!! Checking auto-logout")
  (let* 
      (
       l-ccm-string l-user l-pw l-db
       )
    (when vc-cmsyn-auto-login-logout
      (message "Autologout from CM Synergy...")
;;;       (message "!! AUTO logout")
;;;       (setq l-ccm-string (vc-cmsyn-command-to-string "stop"))
      (setq l-ccm-string (vc-cmsyn-command-to-string "stop"))
      (setenv "CCM_ADDR" nil t) ;; the current session is stopped, make sure we don't try to reuse this
      (message "Autologout from CM Synergy...done")
      (save-match-data (string-match "CM Synergy engine exiting." l-ccm-string)) ;; if successfully
;;;       (message "!! Logged out from Synergy, with outputstring: -%s-" l-ccm-string)
      )
    )
  )

;;;###autoload
(defun vc-cmsyn-co-file ()
  "Check out a file from CM Synergy.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : checks task set before trying to check-out
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-cmsyn-check-login)
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
    (vc-cmsyn-run-command l-message (format "co %s" (vc-cmsyn-platformity-path l-filename)) 'vc-cmsyn-sentinel-co-file nil t)
    ;;     (vc-cmsyn-report l-message)
;;;;     (vc-cmsyn-show-buffer "Starting check out...")
    ;;     (setq comint-output-filter-functions
    ;;           (push 'shell-strip-ctrl-m
    ;;                 comint-output-filter-functions))
    ;;     (setq l-proc (start-process "ccm" (vc-cmsyn-buffer)
    ;; 					      vc-cmsyn-exe-name "co" l-filename))
    ;;     (vc-cmsyn-map-proc-and-buffer l-proc (current-buffer))
    ;; ;;     (message "!! Buffer co3: -%s-, l-filename: -%s-" (file-name-directory (buffer-file-name)) l-filename)
    ;;     (set-process-sentinel l-proc 'vc-cmsyn-sentinel-co-file)
    ;; ;;     (set-process-filter l-proc 'vc-cmsyn-process-filter)
    )
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
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
    (vc-cmsyn-run-command l-message (format "co %s" (vc-cmsyn-platformity-path l-filename)) 'vc-cmsyn-sentinel-co-directory nil t)
    )
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
  (let*
      (
       (l-filename (buffer-file-name))
       (l-message (format "Starting undo check out of %s..." l-filename))
       l-proc
       )
    (vc-cmsyn-run-command l-message (format "unuse -replace -delete %s" (vc-cmsyn-platformity-path l-filename)) 'vc-cmsyn-sentinel-undo-co-file nil t)
;;    (vc-cmsyn-run-command l-message (format "unuse -replace -delete %s" l-filename) 'vc-cmsyn-sentinel-ci-file)
;;     (vc-cmsyn-report l-message)
;; ;;     (vc-cmsyn-show-buffer "Undoing check out...")
;;     (setq comint-output-filter-functions
;;           (push 'shell-strip-ctrl-m
;;                 comint-output-filter-functions))
;;     (setq l-proc (start-process "ccm" (vc-cmsyn-buffer)
;; 					      vc-cmsyn-exe-name "unuse -replace -delete" l-filename))
;;     (vc-cmsyn-map-proc-and-buffer l-proc (current-buffer))
;;     (set-process-sentinel l-proc 'vc-cmsyn-sentinel-ci-file)
    )
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
  (let*
      (
       (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
       (l-message (format "Starting undo check out of %s..." l-filename))
       l-proc
	)
    (vc-cmsyn-run-command l-message (format "unuse -replace -delete %s" (vc-cmsyn-platformity-path l-filename)) 'vc-cmsyn-sentinel-undo-co-directory nil t)
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-register-file (&optional p-dont-check-task)
  "Creates the current file in CM Synergy, only asks the user for a type and version if configured to do so.
  Author        : Realworld Systems (GR).
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-cmsyn-check-login)
;;   (when vc-cmsyn-check-default-task-set-p (vc-cmsyn-check-task-set t))
  (let* ((l-type	      (when vc-cmsyn-query-create-file-type (read-string "Type of the file: " "")))
	 (l-version	      (when vc-cmsyn-query-create-file-version (read-string "Version of the file: " "")))
	 (l-filename	      (buffer-file-name))
;;	 (l-filename	      (vc-cmsyn-platformity-path (buffer-file-name)))
;; 	 (l-filename	      (file-name-nondirectory (buffer-file-name)))
	 (l-create-command    (if l-type (format "create -type %s %s" l-type (vc-cmsyn-platformity-path l-filename)) (format "create %s" (vc-cmsyn-platformity-path l-filename))))
	 (l-attr-command      (when l-version (format "attr -modify version -value %s %s" l-version (vc-cmsyn-platformity-path l-filename))))
	 (l-message	      (format "Starting registration of %s..." l-filename))
	 )
    (vc-cmsyn-run-command l-message l-create-command 'vc-cmsyn-sentinel-register-file nil (not p-dont-check-task))
;;     (vc-cmsyn-report l-message)
;; ;;     (vc-cmsyn-show-buffer)
;;     (pop-to-buffer (vc-cmsyn-buffer))
    (when l-attr-command (vc-cmsyn-run-command "Setting version..." l-attr-command)))
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-create-directory (&optional p-directory p-type)
  "Creates the current directory in CM Synergy, *with* files in it, evt. recursive.
  Author        : Realworld Systems (GR).
  Date          : Apr/2003
  Parameters    : P-DIRECTORY: if supplied, this directory will be registered, otherwise dir will be current directory.
  Returns       : "
  (interactive)
  (vc-cmsyn-check-login)
  (let*
      (
       (l-type			(or p-type (when vc-cmsyn-query-create-file-type (read-string "Type of the files in the Directory: " ""))))
       (l-version		(when vc-cmsyn-query-create-file-version (read-string "Version of the Directory: " "")))
       (l-dir			(or p-directory
				    (if (equal major-mode 'dired-mode)
					(if (listp dired-directory) (car dired-directory) dired-directory)
				      (file-name-directory (buffer-file-name)))))
       (l-attr-command		(when l-version (format "attr -modify version -value %s %s" l-version (vc-cmsyn-platformity-path l-dir))))
       (l-recursive-p		(or p-directory (y-or-n-p (format "Recursive Register %s?" l-dir))))
       (l-files			(delq nil (directory-files l-dir t "\\(^[^\\.]\\|^\\.[^.]\\)")))
       ;;        (l-check-task-set	(not p-directory)) ;;when called recursively the check has been done already
       l-create-command l-string l-file l-ok?
       )
    ;; ----------
    ;; 1st this directory
    ;; ----------
    (mapcar (lambda (p-regexp)
	    (when (string-match p-regexp l-dir) (error "Directory %s should not be registered in Synergy according to a filter defined in `vc-cmsyn-register-directory-and-files-filter'" l-dir))
	    ) vc-cmsyn-register-directory-and-files-filter)
    (message "Registering Directory: -%s-..." l-dir)
;;;     (setq l-string (vc-cmsyn-command-to-string "create" "-type" "dir" (vc-cmsyn-platformity-path l-dir))) ;; have to do this sync, because of multipe actions
    (setq l-create-command	(format "create -type dir %s" (vc-cmsyn-platformity-path l-dir)))
    (setq l-string (vc-cmsyn-command-to-string l-create-command)) ;; have to do this sync, because of multipe actions
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
		) vc-cmsyn-register-directory-and-files-filter)
      (when l-ok?
	(if (file-directory-p i-file)
	    (when l-recursive-p
	      (vc-cmsyn-create-directory i-file l-type) ;; recursive call
	      )
	  (setq l-file (vc-cmsyn-platformity-path i-file))
	  (message "Registering: %s" l-file)
;;; 	  (setq l-string (if l-type ;; type determined from extension
;;; 			     (vc-cmsyn-command-to-string "create" "-type" l-type l-file)
;;; 			   (vc-cmsyn-command-to-string "create" l-file)))
	  (setq l-create-command
		(if l-type (format "create -type %s %s" l-type l-file)
		  (format "create %s" l-file))) ;; type determined from extension
	  (message "Registering: %s" l-file)
	  (setq l-string (vc-cmsyn-command-to-string l-create-command)) ;; have to do this sync, because of multipe actions
	  (save-excursion (set-buffer (vc-cmsyn-buffer)) (goto-char (point-max)) (insert "\n" l-string "\n"))
	  (when (string-match vc-cmsyn-warning-error-output-regexp l-string) (error "Failed Registering %s, see Synergy output-buffer for details" l-dir))
	  (when l-version
;;; 	    (setq l-string (vc-cmsyn-command-to-string "attr" "-modify" "version" "-value" l-version l-file))
	    (setq l-attr-command (format "attr -modify version -value %s %s" l-version l-file))
	    (setq l-string (vc-cmsyn-command-to-string l-attr-command))
	    (save-excursion (set-buffer (vc-cmsyn-buffer)) (goto-char (point-max)) (insert "\n" l-string "\n"))
	    (when (string-match vc-cmsyn-warning-error-output-regexp l-string) (error "Failed Setting version %s at %s" l-version l-file))
	    )
	  (when vc-cmsyn-register-checks-in-p
	    (message "Checking in: %s" l-file)
;;; 	    (setq l-string (vc-cmsyn-command-to-string "ci" "-c" "\"Checked in new file after registering\"" l-file))
	    (setq l-string (vc-cmsyn-command-to-string (format "ci -c \"Checked in new file after registering\" %s" l-file)))
	    )
	  )
	)
      )
    (message "Registering Directory: -%s-...done" l-dir)
    (vc-cmsyn-show-output-buffer)
    )
  (vc-cmsyn-check-logout)
  )

;;(defun vc-cmsyn-create-directory ()
;;  "Creates the current directory in CM Synergy, *with* files in it, evt. recursive.
;;  Author        : Realworld Systems (GR).
;;  Date          : Apr/2003
;;  Parameters    : 
;;  Returns       : "
;;  (interactive)
;;;  (vc-cmsyn-check-login)
;;;;   (when vc-cmsyn-check-default-task-set-p (vc-cmsyn-check-task-set t))
;;  (let* ((l-version	      (when vc-cmsyn-query-create-file-version (read-string "Version of the Directory: " "")))
;;	 (l-filename	      (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
;;	 (l-create-command    (format "create -type dir %s" l-filename))
;;	 (l-attr-command      (when l-version (format "attr -modify version -value %s %s" l-version l-filename)))
;;	 (l-message	      (format "Starting registration of %s..." l-filename))
;;	 (l-recursive-p)
;;	 )
;;    (vc-cmsyn-run-command l-message l-create-command nil nil t)
;;;;     (vc-cmsyn-report l-message)
;;;; ;;     (vc-cmsyn-show-buffer)
;;;;     (pop-to-buffer (vc-cmsyn-buffer))
;;    (when l-attr-command (vc-cmsyn-run-command "Setting version..." l-attr-command)))
;;;   (vc-cmsyn-check-logout)
;;; )

;;;###autoload
(defun vc-cmsyn-ci-file (&optional p-comment-string p-dont-check-task)
  "Check in a file into CM Synergy.
  Date          : Apr/2003
  Parameters    : optional P-COMMENT-STRING: comment-string for check-in, so no query will be performed for it.
  Returns       : 
  Methodology   : checks task set before trying to check-in
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-cmsyn-check-login)
  (when (buffer-modified-p)
    (if (y-or-n-p "Buffer is modified, save buffer first?")
	(save-buffer)
      (message "Current buffer is modified, if you kill the buffer after checkin your changes will be lost!")
      (run-with-timer 3 nil 'message "")
      )
    )
  (vc-cmsyn-check-login)
  (let*
      (
       (l-filename  (buffer-file-name))
       (l-comment   (or p-comment-string (read-string "Checkin-comment (newlines with S-ret, RET when done): ")))
       (l-message   (format "Starting check in of %s..." l-filename))
       (l-command   (if (zerop (length l-comment))
			(format "ci -nc %s" (vc-cmsyn-platformity-path l-filename))
		      (format "ci -c \"%s\" %s" l-comment (vc-cmsyn-platformity-path l-filename))))
       l-proc
       )
;;     (message " !! Running ci command: -%s-" l-command)
    (vc-cmsyn-run-command l-message l-command  'vc-cmsyn-sentinel-ci-file nil (not p-dont-check-task))
;;     (vc-cmsyn-report l-message)
;; ;;     (vc-cmsyn-show-buffer "Starting check in...")
;;     (setq comint-output-filter-functions
;;           (push 'shell-strip-ctrl-m
;;                 comint-output-filter-functions))
;;     (setq l-proc (start-process "ccm" (vc-cmsyn-buffer)
;;                                  vc-cmsyn-exe-name "ci -nc" l-filename))
;;     (vc-cmsyn-map-proc-and-buffer l-proc (current-buffer))
;;     (set-process-sentinel l-proc 'vc-cmsyn-sentinel-ci-file)
    )
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
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
			(format "ci -nc %s" (vc-cmsyn-platformity-path l-filename))
		      (format "ci -c \"%s\" %s" l-comment (vc-cmsyn-platformity-path l-filename))))
       l-proc
       )
    (vc-cmsyn-run-command l-message l-command 'vc-cmsyn-sentinel-ci-directory nil t)
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-history-file-graphics ()
  "Display file history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-cmsyn-check-login)
  (let* (
	 (l-filename (buffer-file-name))
	 (l-command (format "history %s -g" (vc-cmsyn-platformity-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command 'vc-cmsyn-sentinel-history-graphics)
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-history-file-details ()
  "Display file history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-cmsyn-check-login)
  (let* (
	 (l-filename (buffer-file-name))
	 (l-command (format "history %s" (vc-cmsyn-platformity-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command)
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-history-directory-graphics ()
  "Display directory history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-cmsyn-check-login)
  (let* (
	 (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
	 (l-command (format "history %s -g" (vc-cmsyn-platformity-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command 'vc-cmsyn-sentinel-history-graphics)
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-history-directory-details ()
  "Display directory history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-cmsyn-check-login)
  (let* (
	 (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
	 (l-command (format "history %s" (vc-cmsyn-platformity-path l-filename)))
	 )
    (vc-cmsyn-run-command "Retrieving history..." l-command)
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-ci-task ()
  "Checks in the default task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : "
  (interactive)
  (vc-cmsyn-check-login)
  (let* 
      ((l-message "Starting check in Task...")
       (l-comment (read-string "Checkin-comment (newlines with S-RET, RET when done): " ))
       l-proc
       )
    (vc-cmsyn-run-command l-message (format "task -checkin default -comment \"%s\"" l-comment) 'vc-cmsyn-sentinel-ci-task nil t)
;;     (vc-cmsyn-report l-message)
;; ;;     (vc-cmsyn-show-buffer "Starting check in Task...")
;;     (setq l-proc (start-process "ccm" (vc-cmsyn-buffer)
;; 					      vc-cmsyn-exe-name "task" "-checkin" "default" l-filename))
;;     (vc-cmsyn-map-proc-and-buffer l-proc (current-buffer))
;;     (set-process-sentinel l-proc 'vc-cmsyn-sentinel-ci-task)
    )
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
;;   (vc-cmsyn-show-buffer)
  (vc-cmsyn-run-command "Retrieving Current Default Task" "task -default")
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
;;   (vc-cmsyn-show-buffer)
  (let* 
      (
       (l-string (vc-cmsyn-default-task t)) ;; error when no task set
       (l-list (split-string l-string ":"))
       (l-task (car l-list)) ;; the nr.
       )
    (vc-cmsyn-run-command (format "Retrieving files for task %s..." l-string) (format "task -show objects -f \"%%objectname\" %s" l-task))
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-open-task-files ()
  "Load the differences of this task.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-cmsyn-check-login)
  (message "Retrieving files for task ...")
  (let* 
      (
       (l-string	      (vc-cmsyn-default-task t)) ;; error when no task set or failed to retrieve
       (l-dummy		      (progn (message "Retrieving files for task %s..." l-string) l-string))
       (l-list		      (split-string l-string ":"))
       (l-task		      (car l-list)) ;; the nr.
;;;        (l-objects-string      (vc-cmsyn-command-to-string "task" "-show" "objects" "-f" "\"%%objectname@:@%%status\"" l-task))
       (l-objects-string      (vc-cmsyn-command-to-string (format "task -show objects -f \"%%objectname@:@%%status\" %s" l-task)))
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
						     (l-objectversion (split-string (car l-name-parts) vc-cmsyn-version-object-separator))
						     (l-file-leaf     (car l-objectversion))
						     (l-version	      (nth 1 l-objectversion))
						     (l-extension     (file-name-extension l-file-leaf))
;;; 						     (l-type	      (when l-extension (intern l-extension)))
						     (l-type-data     (when (not (equal l-extension "dir")) (vc-cmsyn-patch-type-data-for-file-type l-extension)))
;;; 						     (l-type-data     (when (not (equal l-type "dir")) (assoc l-type vc-cmsyn-patch-types)))
						     (l-object-query  (when l-type-data (format "\"name= '%s'  and version= '%s' and type='%s' and instance='%s' and is_hist_leaf()\"" l-file-leaf l-version (nth 1 l-name-parts) (nth 2 l-name-parts))))
;;; 						     (l-latest-test   (when l-type-data (vc-cmsyn-command-to-string "query" l-object-query)))
						     (l-latest-test   (when l-type-data (vc-cmsyn-command-to-string (format "query %s" l-object-query))))
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
;;       (message "!! Object: -%s-" i-object-name)
      (setq l-list (vc-cmsyn-object-name-2-path i-object-name nil t l-project))
      (setq l-path (nth 0 l-list))
      (when (not l-project) (setq l-project (nth 1 l-list)))
      (when l-path (find-file l-path))
      )
    (message "Retrieving files for task %s...done" l-string)
    )
  (vc-cmsyn-check-logout)
  )

(defun vc-cmsyn-default-task (&optional p-error-p)
  "Retrieves the default task from Synergy.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : P-ERROR-P: if t an error will be generated if no task set
  Returns       : string, nil or generates error"
  (let* 
      (
;;;        (l-string	      (vc-cmsyn-command-to-string "task" "-default"))
       (l-string	      (vc-cmsyn-command-to-string "task -default"))
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
  (vc-cmsyn-check-login)
  (let* ((l-filename (buffer-file-name))
	 (l-prop-command (format "prop %s" (vc-cmsyn-platformity-path l-filename))))
;;     (vc-cmsyn-show-buffer)
    (vc-cmsyn-run-command "Properties..." l-prop-command))
  (vc-cmsyn-check-logout)
  )

(defun vc-cmsyn-create-task ()
  "Create a new task"
  (interactive)
  (vc-cmsyn-check-login)
  (vc-cmsyn-report "Starting create Task...")
;;   (vc-cmsyn-show-buffer "Starting create Task...")
  (let*
      (
       (l-proc (start-process vc-cmsyn-exe-name (vc-cmsyn-buffer)
					    vc-cmsyn-exe-name "create_task" "-g")))
    )
  (vc-cmsyn-check-logout)
  )

;;;###autoload
(defun vc-cmsyn-set-ccmAddr ()
  "Read ccmAddr from the user"
  (interactive)
  (vc-cmsyn-check-login)
  (let*
      (
       (l-ccm-addr (read-string "Enter CCM_ADDR (like <machine-name>:<number><ip-address-Synergy-server>: " (or (getenv "CCM_ADDR") "")))
       )
    (setenv "CCM_ADDR" l-ccm-addr))
  (vc-cmsyn-check-logout)
  )

;;(defun vc-cmsyn-create ()
;;  "Creates the current file in CM Synergy, asks the user for a type and version"
;;  (interactive)
;;  (vc-cmsyn-check-login)
;;  (let* ((type (read-string "Type of the file: " ""))
;;	 (version (read-string "Version of the file: " ""))
;;	 (filename (file-name-directory (buffer-file-name)))
;;	 (create-command (format "create -type %s %s"
;;				 type filename))
;;	 (attr-command (format "attr -modify version -value %s %s"
;;			       version filename)))
;;
;;    (vc-cmsyn-show-buffer)
;;    (vc-cmsyn-run-command "Creating file..." create-command)
;;    (vc-cmsyn-run-command "Setting version..." attr-command))
;;   (vc-cmsyn-check-logout)
;;)

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
  (and vc-cmsyn-check-default-task-set-p p-check-task-set-p (vc-cmsyn-check-task-set t))
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
;;;     (message "!! env now: -%s-" (getenv "ccm_addr"))
;;;     (message "!! task-string: -%s-, p-command: -%s-" (vc-cmsyn-command-to-string "ccm task -default") p-command)
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

;; ----------
;; VC version of run-command
;; ----------
;;(defun vc-cmsyn-run-command (p-info p-command &optional p-sentinel p-filter p-check-task-set-p)
;;  "Call the CM Synergy executable with the parameter-line P-COMMAND, supplying info with P-INFO.
;;  Date          : Apr/2003
;;  Parameters    : P-INFO: Message to report
;;                  P-COMMAND: part of command-line after the name of the executable
;;  Returns       : 
;;  Author        : Realworld Systems (GR)."
;;  ;; ----------
;;  ;; check task set, if applicable
;;  ;; ----------
;;  (and vc-cmsyn-check-default-task-set-p p-check-task-set-p (vc-cmsyn-check-task-set t))
;;  (let* 
;;      ((l-sentinel (or p-sentinel 'vc-cmsyn-sentinel))
;;       l-proc)
;;    (vc-cmsyn-report (format "\n%s\n" p-info))
;;    (setq comint-output-filter-functions
;;	  (push 'shell-strip-ctrl-m
;;		comint-output-filter-functions))
;;    (message "!! Running command: -%s-, current-dir: -%s-" p-command default-directory)
;;    (vc-do-command (vc-cmsyn-buffer) 'async "ccm" (buffer-file-name) (split-string p-command))
;;    )
;;  )

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
	 (l-buffer		(get-buffer-create vc-cmsyn-buffer-name))
	 )
      ;; (if (and vc-cmsyn-buffer-frame
      ;; 	       (frame-live-p vc-cmsyn-buffer-frame))
      ;; 	  ;; ----------
      ;; 	  ;; check if this still displays the right buffer (user may have
      ;; 	  ;; deleted the buffer)
      ;; 	  ;; ----------
      ;; 	  (progn
      ;; 	    (when (not (equal l-buffer (window-buffer (frame-first-window vc-cmsyn-buffer-frame))))
      ;; 	      (raise-frame vc-cmsyn-buffer-frame)
      ;; 	      (set-window-dedicated-p (current-buffer) nil)
      ;; 	      (switch-to-buffer l-buffer)
      ;; 	      (set-window-dedicated-p (get-buffer-window l-buffer) t)
      ;; 	      (when p-stay-in-current-frame (raise-frame l-current-frame)) 
      ;; 	      ))
      ;; 	;; ----------
      ;; 	;; no (live) frame present, make 1
      ;; 	;; ----------
      ;; 	(set-buffer l-buffer)
      ;; 	(select-frame-set-input-focus (make-frame
      ;; 			     `((width          . ,vc-cmsyn-buffer-frame-width)
      ;; 			       (height         . ,vc-cmsyn-buffer-frame-height)
      ;; 			       (border-width   . 0)
      ;; 			       (menu-bar-lines . 0)
      ;; 			       (tool-bar-lines . 0)
      ;; 			       (unsplittable   . t)
      ;; 			       (minibuffer     . nil)
      ;; 			       (modeline       . nil)
      ;; 			       (title	       . "CM Synergy Output")
      ;; 			       )))
      ;; 	(setq vc-cmsyn-buffer-frame (selected-frame))
      ;; 	(set-window-dedicated-p (get-buffer-window l-buffer) t)
      ;; 	(when p-stay-in-current-frame (raise-frame l-current-frame)) 
      ;; 	)
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

(defun vc-cmsyn-int-update-modeline (p-vc-cmsyn-string)
  "Do put P-VC-CMSYN-STRING in the modeline.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-VC-CMSYN-STRING: the id for this file in the modeline
  Returns       : -"
  (setq vc-cmsyn-modeline-string p-vc-cmsyn-string)
  (set (intern "mode-line-buffer-identification") 'vc-cmsyn-modeline-string)
  (when (get-buffer-window (current-buffer) 0)
    (redraw-modeline)
    )
  )

;;;###autoload
(defun vc-cmsyn-update-modeline ()
  "Show file name, version and status in the modeline, retrieving the information asynchronously so user doesnt' have to wait.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : Partly copied from ccm-update-modeline, made asynchronous
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-cmsyn-check-login)
  (when (buffer-file-name) ;; only if we are in a file there is version-information to display
    (let* ((l-filename (file-name-nondirectory (buffer-file-name)))
	   ;; ----------
	   ;; retrieve the output with proc with filter, mode-line is updated by filter
	   ;; ----------
	   l-proc l-string
	   )
      (setq comint-output-filter-functions
	    (push 'shell-strip-ctrl-m
		  comint-output-filter-functions))
      ;; ----------
      ;; set temp modeline-info (while waiting for the correct info from ccm)
      ;; ----------
      (setq l-string (format "%s-%s" l-filename "CMSynergyRetrievingVersion..."))
      (vc-cmsyn-int-update-modeline l-string)
;;       (message "!! direct string: -%s-" (vc-cmsyn-command-to-string (format "ccm ls -f %s" (format "\"%%version@@%%status\" %s" (vc-cmsyn-platformity-path (buffer-file-name))))))
      ;; ----------
      ;; Changed: GR: calling `start-process' seems to give hanging
      ;; processes now and then. Changed to `call-process' therefore
      ;; ----------
      (setq l-string (vc-cmsyn-command-to-string (format "ls -f \"%%version@@%%status\" %s" (vc-cmsyn-platformity-path (buffer-file-name)))))
      ;; ----------
      ;; put the info from CM Synergy in the mode-line
      ;; ----------
      (message "!! CCM VERSION-String: -%s-" l-string)
      (save-match-data
	(let* 
	    (
	     ;; ----------
	     ;; Split the answer at : if there is no error-message
	     ;; there may be text before the answer: chop off, the answer is a line
	     ;; of its own, the last
	     ;; ----------
	     (l-ccm-version (if (and (string-match "\\(.*\n\\)*\\([^@\n]+\\)@@\\([^@\n]+\\)\n" l-string)
				     (not (string-match vc-cmsyn-warning-error-output-regexp l-string))
				     )
				(format "%s:%s"(match-string 2 l-string) (match-string 3 l-string))
			      "CMSynergyVersion??"))
	     (l-buffer (current-buffer))
	     (l-writable-string (if buffer-read-only "-" ":"))
	     (l-filename (vc-cmsyn-platformity-path (file-name-nondirectory (buffer-file-name))))
	     )
	  (vc-cmsyn-int-update-modeline (format "%s%s%s" l-filename l-writable-string l-ccm-version))
	  )
	)
      )
    )
  (vc-cmsyn-check-logout)
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
    (when (and vc-cmsyn-command-end-jump-to-output-buffer-p
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
	  (when (re-search-forward (format "%s.*$" vc-cmsyn-parallel-versions-string ) nil t)
	    (x-popup-dialog t (list (match-string 0) (cons "Ok" t)))
	    )
	  )
	)
      )
    (set-buffer l-buffer)
    (when (string-match "finished" p-what) ;; carefull with blanks and newlines in p_what!
      (revert-buffer t t) ;; get the version that was checked out from CM Synergy
;;;      (find-file (buffer-name l-buffer)) ;; get the version that was checked out from CM Synergy
      ;; ----------
      ;; evt. reset the directory-status immediately again: other files in
      ;; the directory may be checked out for other tasks, thus it's better
      ;; not to have the directory marked for check-out for *this* task (which
      ;; is the default behaviour for personal work-areas)
      ;; When the directory is not checked out, this will just give the
      ;; message "Object '...' is already in the 'released' state.", no
      ;; problem so we don't test for being checked out 1st!
      ;; ----------
      (when vc-cmsyn-auto-check-in-directory-p
	(let* 
	    (
	     (l-filename (vc-cmsyn-platformity-path (buffer-file-name)))
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
;;;     (vc-cmsyn-check-status-buffer) ==> via update-mode-line in ( vc-cmsyn-sentinel)
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
    (if vc-cmsyn-register-checks-in-p
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
      (while (re-search-forward (format "%s.*$" vc-cmsyn-parallel-versions-string ) nil t)
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
	(when (re-search-forward (format "%s.*$" vc-cmsyn-parallel-versions-string ) nil t)
	  (x-popup-dialog t (list (match-string 0) (cons "Ok" t)))
	  )
	)
      )
    (set-buffer l-buffer)
    (revert-buffer t t)
;;     (vc-cmsyn-check-status-buffer)
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
      (while (re-search-forward (format "%s.*$" vc-cmsyn-parallel-versions-string ) nil t)
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
;;     (vc-cmsyn-sentinel p-process p-what) ;; cleanup, evt. remove C-m's
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
;;     (vc-cmsyn-check-status-buffers)
    (vc-cmsyn-clear-buffer-mapping-for-proc p-process)
;;     (vc-cmsyn-sentinel p-process p-what) ;; cleanup, evt. remove C-m's
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
      (while (re-search-forward (format "%s.*$" vc-cmsyn-parallel-versions-string ) nil t)
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
;;   (vc-cmsyn-sentinel p-process p-what)
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

(defun vc-cmsyn-sentinel-start-classic-gui (p-process p-what)
  "Function running when the ccm login process is done.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROCESS: ccm process
                  P-WHAT: change in process
  Returns       : "
  (save-excursion
    (let* 
	(
	 l-ccm-addr l-string
	 )
      (set-buffer (process-buffer p-process))
      (goto-char (point-max))
      (if (not (re-search-backward "address is \\(.+\\)\\.?$" nil t))
	  (setenv "CCM_ADDR" nil t) ;; not found, ??, make sure to unset for now!
	(setq l-ccm-addr (match-string 1))
	(when (string-match "\\(\.[ \t]*\\)\\'" l-ccm-addr) (setq l-ccm-addr (replace-match "" t t l-ccm-addr 1))) ;; chopp off last dot
	(setenv "CCM_ADDR" l-ccm-addr)
	)
      )
    )
  (vc-cmsyn-sentinel p-process p-what) ;; standard finish
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
;;; 	 (l-string (vc-cmsyn-command-to-string "status")) ;; find out ccm_addr cli
	 (l-string (vc-cmsyn-command-to-string "status")) ;; find out ccm_addr cli
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
;;;   (vc-cmsyn-update-modeline) ;; update-mode-line calls this one!
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
(defun vc-cmsyn-start ()
  "Start a ccm session.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : "
  (interactive)
  (if vc-cmsyn-use-developers-gui-p
      (let* 
	  (
	   (l-current-buffer (current-buffer))
	   (l-proc-buffer (vc-cmsyn-buffer))
	   l-proc
	   )
	;; ----------
	;; do not call vc-cmsyn-run-command in this case since that would call
	;; cmm and we need to call cmsynergy now!
	;; ----------
	(setq l-proc (start-process vc-cmsyn-exe-name l-proc-buffer vc-cmsyn-developers-gui-exe-name))
	(vc-cmsyn-map-proc-and-buffer l-proc l-current-buffer)
	(set-process-sentinel l-proc 'vc-cmsyn-sentinel-start-developers-gui)
	)
    (when vc-cmsyn-auto-login-logout ;; make sure when using the GUI there is no auto-login but connection is with the GUI
      (message-box "Auto-login-logout switched off becauswe of GUI-interface")
      (setq vc-cmsyn-auto-login-logout nil)
      ) 
    (vc-cmsyn-run-command "Logging in to CM Synergy..." "start" 'vc-cmsyn-sentinel-start-classic-gui)
    ;;  (vc-cmsyn-run-command "Logging in to CM Synergy..." "start -g -q" 'vc-cmsyn-sentinel-start)
    )
  ;; ----------
  ;; The start-panel *does* come up above the current window so in this
  ;; case our emacs frame doesn't have to be iconized
  ;; ----------
  ;; (when vc-cmsyn-iconify-frame-when-ccm-gui (iconify-frame))
  ;;   (start-process "ccm" (vc-cmsyn-buffer) vc-cmsyn-exe-name "start")
  )

;;;###autoload
(defun vc-cmsyn-stop ()
  "Stop a ccm session.
  Author        : Realworld Systems (GR)
  Date          : Mar/2004
  Parameters    : 
  Returns       : "
  (interactive)
  (vc-cmsyn-run-command "Logging out from CM Synergy..." "stop")
  )

;;;###autoload
(defun vc-cmsyn-select-task ()
  "Select a CM Synergy task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-cmsyn-check-login)
  (if vc-cmsyn-use-developers-gui-p
      (vc-cmsyn-select-task-cli)
    (vc-cmsyn-select-task-gui)
    )
  (vc-cmsyn-check-logout)
  )

(defun vc-cmsyn-select-task-cli ()
  "Select a CM Synergy task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (message "!! vc-cmsyn-select-task-cli")
  (vc-cmsyn-check-login)
  ;; ----------
  ;; Use the command-interface to retrieve the tasks which will be
  ;; presented 'tabbable' to the user to select 1
  ;; ----------
  (message "  !! vc-cmsyn-select-task-cli2")
  (let* 
      (
;;;        (l-status-string (vc-cmsyn-command-to-string "status"))
;;;        (a (message "  !! status-string+ -%s-" l-status-string))
      (l-status-string (vc-cmsyn-command-to-string "status"))
       (l-user (when (string-match "^Sessions for user[ \t]+\\([^:]+\\):" l-status-string) (match-string 1 l-status-string)))
;;;        (b (message "  !! user: -%s-" l-user))
;;;        (l-task-list-string (when l-user (vc-cmsyn-command-to-string "query" (format "\"status='task_assigned' and resolver='%s'\"" l-user) "-f" "\"%%task_number:%%task_description:%%task_synopsis\"" )))
;;;        (c (message "  !! stasklist: -%s-" l-task-list-string))
       (l-task-list-string (when l-user (vc-cmsyn-command-to-string (format "query \"status='task_assigned' and resolver='%s'\" -f \"%%task_number:%%task_description:%%task_synopsis\"" l-user))))
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
;;;       (vc-cmsyn-command-to-string "task" "-default" l-task)
     (vc-cmsyn-command-to-string (format "task -default %s" l-task))
      (message-box "Task set to %s!" l-task)
      )
    )
  (vc-cmsyn-check-logout)
  )

(defun vc-cmsyn-select-task-gui ()
  "Select a CM Synergy task.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (interactive)
  (vc-cmsyn-check-login)
  ;;   (vc-cmsyn-show-buffer "Calling select Task GUI...")
  (vc-cmsyn-run-command "Calling select Task GUI..." "task -default -g" )
  ;;  (let* ((proc (start-process "ccm" (vc-cmsyn-buffer)
  ;;                                           vc-cmsyn-exe-name "task" "-default" "-g")))
  ;;    )
  (when vc-cmsyn-iconify-frame-when-ccm-gui (iconify-frame))
  (vc-cmsyn-check-logout)
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
;;;        (l-string (vc-cmsyn-command-to-string "task" "-default"))
      (l-string (vc-cmsyn-command-to-string "task -default" ))
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
;; (define-key global-map (kbd "C-c C-m d") 'vc-cmsyn-ci-task)
(define-key vc-cmsyn-mode-map (kbd "C-c C-m h") 'vc-cmsyn-history-file-graphics)
(define-key global-map (kbd "C-c C-m d") 'vc-cmsyn-show-default-task)
(define-key global-map (kbd "C-c C-m t") 'vc-cmsyn-ci-task)
;; (define-key global-map (kbd "C-c C-m t") 'vc-cmsyn-create-task)
(define-key global-map (kbd "C-c C-m s") 'vc-cmsyn-select-task)
(define-key global-map (kbd "C-c C-m p") 'vc-cmsyn-properties)
(define-key global-map (kbd "C-c C-m a") 'vc-cmsyn-about)
(define-key global-map (kbd "C-c C-m l") 'vc-cmsyn-start) ;; login
(define-key vc-cmsyn-mode-map (kbd "C-c C-m r") 'vc-cmsyn-register-file) ;; register

					; Minor Mode
;; (defvar vc-cmsyn-mode nil
;;   "Minor mode for editing CM Synergy controlled files")

;; (defun vc-cmsyn-mode (&optional arg)
;;   "Minor mode for editing CM Synergy controlled files"
;;   (interactive)
;;   (setq vc-cmsyn-mode
;; 	(if (null arg) (not vc-cmsyn-mode)
;; 	  (> (prefix-numeric-value arg) 0)))
;;   )

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
  (vc-cmsyn-check-login)
  (let* 
      (
;;;        (l-string (vc-cmsyn-command-to-string "status"))
      (l-string (vc-cmsyn-command-to-string "status"))
       )
    (save-match-data
      (when (string-match "^Current project:[ \t]+\'\\([^\']+\\)\'" l-string)
	(match-string 1 l-string)
	)
      )
    )
  (vc-cmsyn-check-logout)
  )

;; ----------
;; Function below not used at the moment!
;; ----------
;;; (defun vc-cmsyn-select-project ()
;;;   "Let user select a project from a list of projects.
;;;   Author        : Realworld Systems (GR)
;;;   Date          : Apr/2003
;;;   Parameters    : 
;;;   Returns       : string: <project-name>:%version:<release>:<owner>"
;;;   ;; ----------
;;;   ;; let user select a project
;;;   ;; ----------
;;;   (save-match-data
;;;     (let* 
;;; 	(
;;; 	 (l-string (vc-cmsyn-command-to-string "-p" "-f" "\"%project:%version:%release:owner\""))
;;; ;;;	 (l-string (vc-cmsyn-command-to-string (format "%s -p -f \"%project:%version:%release:owner" vc-cmsyn-exe-name)))
;;; 	 l-projects
;;; 	 )
;;;       (when (not (string-match "^[ \t]*[0-9]+\)[ \t]*\\(.*\\)$" l-string)) (error "No projects found!"))
;;;       (setq l-projects (delq nil
;;; 			     (mapcar
;;; 			      (lambda (p-line)
;;; 				(when (string-match "^[ \t]*[0-9]+\)[ \t]*\\(.*\\)$" p-line)
;;; 				  (match-string 1 p-line)
;;; 				  )
;;; 				)
;;; 			      (split-string l-string "\n"))))
;;;       (completing-read  "Select a Project [tab to complete]: " l-projects)
;;;       )
;;;     )
;;;   )

(defvar vc-cmsyn-object-name-regexp
  (format "[^%s]+%s\\([^:]+\\):[^:]+:[^:]+" vc-cmsyn-version-object-separator vc-cmsyn-version-object-separator)
  "*Regexp to match an object-name of CM Synergy.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  )

(defvar vc-cmsyn-object-name-list-output-regexp
  (format "^[0-9]+\)[ \t]+%s+" vc-cmsyn-object-name-regexp)
;;   (format "^[0-9]+\)[ \t]+[^%s]+%s\\([^:]+\\):[^:]+:[^:]+" vc-cmsyn-version-object-separator vc-cmsyn-version-object-separator)
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
  subexp 1 is fileleafname<`vc-cmsyn-version-object-separator'>version
  subexp 2 is project-name
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  )

(defvar vc-cmsyn-file-ci-output-regexp
  "Checked in[ \t]*'\\([^']+\\)'[ \t]*to[ \t]*'\\([^']+\\)'"
  "*Regexp to match correct file-ci output from CM Synergy.
  subexp 1 is fileleafname<`vc-cmsyn-version-object-separator'>version
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
  (vc-cmsyn-check-login)
  (when (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-v1            (read-string "Older version: "))
	 (l-v2            (read-string "Newer version: "))
;;; 	 (l-object-name   (vc-cmsyn-command-to-string "ls" "-f" "\"%%objectname\"" (vc-cmsyn-platformity-path l-file)))
	 (l-object-name   (vc-cmsyn-command-to-string (format "ls -f \"%%objectname\" %s" (vc-cmsyn-platformity-path l-file))))
	 l-low-buffer l-high-buffer l-string1 l-string2 l-file1 l-file2
	 )
      (if (not (string-match vc-cmsyn-object-name-regexp l-object-name))
	  (error "Couldnt retrieve the right file-information from CMSynergy: %s" l-object-name)
	(save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
	(setq l-file1 (replace-match l-v1 nil nil l-object-name 1))
	(setq l-file2 (replace-match l-v2 nil nil l-object-name 1))
;;; 	(setq l-string1 (vc-cmsyn-command-to-string "cat" l-file1))
	(setq l-string1 (vc-cmsyn-command-to-string (format "cat %s" l-file1)))
	(when (string-match vc-cmsyn-warning-error-output-regexp l-string1)
	  (error (format "CM Synergy Error: %s" l-string1)))
;;; 	(setq l-string2 (vc-cmsyn-command-to-string "cat" l-file2))
	(setq l-string2 (vc-cmsyn-command-to-string (format "cat %s" l-file2)))
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
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
  (when  (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-v1            (read-string "Other version: "))
;;; 	 (l-object-name   (vc-cmsyn-command-to-string "ls" "-f" "\"%%objectname\"" (vc-cmsyn-platformity-path l-file)))
	 (l-object-name   (vc-cmsyn-command-to-string (format "ls -f \"%%objectname\" %s" (vc-cmsyn-platformity-path l-file))))
	 (l-high-buffer    (current-buffer))
	 l-low-buffer l-string1 l-string2 l-file1 l-file2
	 )
      (if (not (string-match vc-cmsyn-object-name-regexp l-object-name))
	  (error "Couldnt retrieve the right file-information from CMSynergy: %s" l-object-name)
	(save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
	(setq l-file1 (replace-match l-v1 nil nil l-object-name 1))
;;; 	(setq l-string1 (vc-cmsyn-command-to-string "cat" l-file1))
	(setq l-string1 (vc-cmsyn-command-to-string (format "cat %s" l-file1)))
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
  (vc-cmsyn-check-logout)
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
  (vc-cmsyn-check-login)
  (when  (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
;;; 	 (l-object-name   (vc-cmsyn-command-to-string "ls" "-f" "\"%%objectname\"" (vc-cmsyn-platformity-path l-file)))
	 (l-object-name   (vc-cmsyn-command-to-string (format "ls -f \"%%objectname\" %s" (vc-cmsyn-platformity-path l-file))))
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
;;;       (setq l-string1 (vc-cmsyn-command-to-string "cat" l-parent-object-name))
     (setq l-string1 (vc-cmsyn-command-to-string (format "cat %s" l-parent-object-name)))
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
  (vc-cmsyn-check-logout)
  )

(defun vc-cmsyn-parent-4-part-name (p-4-part-name)
  "Retrieve the parent-4-part-name of file in current-buffer.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : 
  Returns       : String or list with strings (if more than 1 ancessor)"
  (let* 
      (
;;;        (l-parent-object-name-string (vc-cmsyn-command-to-string "query" (format "\"is_predecessor_of ('%s')\"" p-4-part-name) "-f" "\"%%objectname\""))
      (l-parent-object-name-string (vc-cmsyn-command-to-string (format "query \"is_predecessor_of ('%s')\" -f \"%%objectname\"" p-4-part-name)))
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
;;        (l-project-name	 (save-match-data (string-match (format "^\\([^%s]+\\)%s" vc-cmsyn-version-object-separator vc-cmsyn-version-object-separator) p-project-name-version) (match-string 1 p-project-name-version)))
;;;        (l-wa-string	 (vc-cmsyn-command-to-string "attr" "-show" "wa_path" "-project" p-project-name-version)) ;; workarea output
      (l-wa-string	 (vc-cmsyn-command-to-string (format "attr -show wa_path -project %s" p-project-name-version))) ;; workarea output
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
;;;        (l-ccm-string (vc-cmsyn-command-to-string "finduse" p-4-part-name))
      (l-ccm-string (vc-cmsyn-command-to-string (format "finduse %s" p-4-part-name)))
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
;;; 	  (setq l-projects-alist
;;; 		(delq nil (mapcar
;;; 			   (lambda (p-list)
;;; 			     (let* 
;;; 				 (
;;; 				  (l-4-part-name (format "%s:project:1" (car p-list)))
;;; 				  (l-status (vc-cmsyn-command-to-string (format "%s attr -show status %s" vc-cmsyn-exe-name l-4-part-name)))
;;; 				  )
;;; 			       (save-match-data ;; chop off the end \n
;;; 				 (string-match "\\([^\n]+\\)\n*" l-status)
;;; 				 (setq l-status (match-string 1 l-status))
;;; 				 )
;;; 			       (when (member l-status (list "working" "visible" "shared")) p-list)
;;; 			       )
;;; 			     )
;;; 			   l-projects-alist)))
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
;;;	      (when (and (not l-project) (not p-no-error-p)) (error "No project in working|visible|shared status found for %s (with cached project %s)" p-4-part-name p-project-name))
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
;;;	       (error "%s is used in more than 1 project, can't determine a unique Path for it!" p-4-part-name)
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
    (setq l-rel-path	    (when l-rel-path-version (car (split-string l-rel-path-version vc-cmsyn-version-object-separator))))
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
  (save-match-data (string-match (format "^\\([^%s]+\\)%s" vc-cmsyn-version-object-separator vc-cmsyn-version-object-separator) p-4-part-name) (match-string 1 p-4-part-name))
  )

(defun vc-cmsyn-base-line-4-part-name-of (p-4-part-name)
  "Retrieve the 4-part-name of the base-line-version of P-4-PART-NAME.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : P-4-PART-NAME: the 4-part-name that we want the base-line-version of
  Returns       : 4-part-name-string or nil"
  (let* 
      (
       (l-list		      (split-string p-4-part-name (format "[%s:]" vc-cmsyn-version-object-separator)))
       (l-ccm-type	      (nth 2 l-list))
       (l-instance	      (nth 3 l-list))
       (l-filename	      (nth 0 l-list))
;;;        (l-use		      (vc-cmsyn-command-to-string "finduse" p-4-part-name))
       (l-use		      (vc-cmsyn-command-to-string (format "finduse %s" p-4-part-name)))
       l-project l-rp-bl-string l-bl-project l-file-1 l-result l-list
       )
    (if (not (string-match "@" l-use))
	(setq l-result nil) ;; not used, or error-message, forget about it
      (setq l-project (car (split-string (nth 1 (split-string l-use "@")) "\n")))
      (setq l-rp-bl-string (vc-cmsyn-command-to-string
;;; 			    "rp" "-show" "bl" l-project ;; format <nr>) <projectname>\n
			    (format "rp -show bl %s" l-project) ;; format <nr>) <projectname>\n
			    ))
      (if (not (string-match "^[0-9]+\)[ \t]+[^ \t]+[ \t]*\n" l-rp-bl-string))
	  (setq l-result nil) ;; not the right output, forget about this? Message?
	(setq l-bl-project (nth 1 (split-string l-rp-bl-string)))
;;;	l-bl-project l-filename l-ccm-type l-instance))
	(setq l-file-1 (vc-cmsyn-command-to-string
;;; 		    "query" (format "\"is_member_of('%s') and name='%s' and type='%s' and instance='%s' \"" l-bl-project l-filename l-ccm-type l-instance) " -f" "\"%%objectname\""
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

;; ----------
;; VC Compatibility
;; Add to handled backends
;; ----------
;; (when (not (member 'cmsyn vc-handled-backends))
;;   (customize-set-variable 'vc-handled-backends (push 'cmsyn vc-handled-backends)))

;;(defun vc-cmsyn-post-command-function (p-command p-file p-flags)
;;  "Function run at the end of vc-do-command from the `vc-post-command-functions-hook', will test if the vc-command was a vc-cmsyn-command..
;;  Author        : Realworld Systems (GR)
;;  Date          : Apr/2003
;;  Parameters    : P-COMMAND: See description of `vc-post-command-functions'
;;                  P-FILE: See description of `vc-post-command-functions'
;;                  P-FLAGS: See description of `vc-post-command-functions'
;;  Returns       : -"
;;  (when (equal p-command "ccm") ;; this is run after *every* call to vc-do-command!
;;    (let* 
;;	(
;;	 (l-file-buffer (find-buffer-visiting p-file))
;;	 )
;;      ;; ----------
;;      ;; *Eventually* set writable / readonly of buffer
;;      ;; ----------      
;;;;       (when (member (car p-flags) '("ci" "checkin" "co" "checkout" "create"))
;;;; 	(vc-cmsyn-check-status-buffer)
;;;; 	)
;;      ;; ----------
;;      ;; jump to output-buffer if told to do so
;;      ;; ----------
;;      (when vc-cmsyn-command-end-jump-to-output-buffer-p
;;	(vc-cmsyn-show-buffer)
;;	)
;;      )
;;    )
;;  )
;;
;;(add-hook 'vc-post-command-functions 'vc-cmsyn-post-command-function)
;;
;;(defalias 'vc-cmsyn-checkin 'vc-cmsyn-ci-file)
;;(defalias 'vc-cmsyn-checkout 'vc-cmsyn-co-file)


(defun vc-cmsyn-responsible-p (p-file &optional p-prev p-absolute)
 "Check if the file P-FILE is located in a CM Synergy workarea"
 (let* ( (l-file     (if p-absolute
			 p-file
		       (vc-cmsyn-unixify-path p-file))       )   ;; unixify
	 (l-path     (file-name-directory l-file)            )
	 (l-expanded (expand-file-name "_ccmwaid.inf" l-path))
	 (l-up       (expand-file-name ".." l-path)          )
	 (l-up-trail (if (string-match "/$" l-up)
			 l-up
		       (concat l-up "/"))                    )
	 (l-prev     (if p-prev p-prev "")                   ) )
   
   ;;(vc-cmsyn-buffer-insert 
   ;; (concat "Scanning Rational Synergy file at: " l-path))

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
;;; 	      (l-object-name		(vc-cmsyn-command-to-string "ls" "-f" "\"%%objectname\"" (vc-cmsyn-platformity-path l-file)))
	      (l-object-name		(vc-cmsyn-command-to-string (format "ls -f \"%%objectname\" %s" (vc-cmsyn-platformity-path l-file))))
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

;; (defun vc-cmsyn-state (p-file)
;;   "Check if this file was changed after checkout, calls ccm diff, is *slow*.
;;   Author        : Realworld Systems (GR)
;;   Date          : Apr/2003
;;   Parameters    : P-FILE: File path to check for change
;;   Returns       : boolean"
;;   (let* 
;;       (
;;        (l-state (vc-file-getprop 'vc-state))
;;        l-state-info
;;        )
;;     (when (not l-state)
;;       (setq l-status (vc-cmsyn-command-to-string (format "%s ls -f \"%%status\" %s" vc-cmsyn-exe-name (buffer-file-name))))
;;       (setq l-state
;; 	    (cond
;; 	     ((not (member l-status vc-cmsyn-checked-out-status-list))
;; 	      t
;; 	      )
;; 	     (t
;; 	      ;; ----------
;; 	      ;; do a diff with the last higher version, slow!
;; 	      ;; ----------
;; 	      )
;; 	     )
;; 	    )
;;       (setq l-state-info (vc-cmsyn-command-to-string (format "ccm diff %s " p-file)))
;; ;;      (setq l-state (vc-cmsyn-command-to-string (format "%s diff %s %s" vc-cmsyn-exe-name p-file (format "%s%s%s" p-file vc-cmsyn-version-object-separator "1"))))
;;       (setq l-state
;; 	    (cond
;; 	     ((and (file-writable-p (p-file))
;; 		   (> (length l-string) 0)
;; 		   )
;; 	      'edited ;; ==> ?? must know if the changes are in the parent or in the child! Could also be 'needs-patch or 'needs-merge!
;; 	      )
;; 	     ((and (not (file-writable-p (p-file)))
;; 		   (zerop (length l-string))
;; 		   )
;; 	      'up-to-date ;; ==> ?? must know if the changes are in the parent or in the child! Could also be 'needs-patch or 'needs-merge!
;; 	      )
;; 	     )
;; 	    )
;;       (when (> (length l-state) 0)
;; 	(setq l-state (make-symbol l-state))
;; 	(vc-file-setprop 'vc-state l-state))
;;       )
;;     (equal l-state 'up-to-date)
;;     )
;;   )

;;(defun vc-cmsyn-state-heuristic ()
;;  "get state-info of current file, called by vc when opening files, 1st sets state as retrievinn, async gets the state and sets in mode-line thten.
;;  Date          : Apr/2003
;;  Parameters    : 
;;  Returns       : 
;;  Methodology   : Partly copied from ccm-update-modeline, made asynchronous
;;  Author        : Realworld Systems (GR)."
;;  (interactive)
;;  (vc-cmsyn-check-login)
;;  (when (buffer-file-name) ;; only if we are in a file there is version-information to display
;;    (let* ((l-filename (file-name-nondirectory (buffer-file-name)))
;;	   ;; ----------
;;	   ;; retrieve the output with proc with filter, mode-line is updated by filter
;;	   ;; ----------
;;	   l-proc l-string
;;	   )
;;      (message "!! vc-cmsyn-update-modeline, Filename 1: -%s-" l-filename)
;;      (setq comint-output-filter-functions
;;	    (push 'shell-strip-ctrl-m
;;		  comint-output-filter-functions))
;;      ;; ----------
;;      ;; set temp modeline-info (while waiting for the correct info from ccm)
;;      ;; ----------
;;      (setq l-string (format "%s-%s" l-filename "CMSynergyRetrievingVersion..."))
;;      (vc-cmsyn-int-update-modeline l-string)
;;      (message "!! Update modeline, file-naem: -%s-, default-dir: -%s-" l-filename default-directory)
;;      (setq l-proc (start-process "vc-cmsyn" nil "ccm" "ls" "-f" (format "\"%%version:%%status\" %s" (buffer-file-name))))
;;;;      (setq l-proc (start-process "vc-cmsyn" nil "ccm" "ls" "-f" (format "\"%%version:%%status\" %s" l-filename)))
;;      (vc-cmsyn-map-proc-and-buffer l-proc (current-buffer))
;;      (set-process-sentinel
;;       l-proc
;;       (lambda (p-proc p-what)
;;	 ;; ----------
;;	 ;; check modeline in case no output from process!
;;	 ;; ----------
;;	 (message "!! Update modeline sentinel: -%s-" p-what)
;;	 (save-match-data 
;;	   (let* 
;;	       (
;;		(l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-proc))
;;		l-filename
;;		)
;;	     (when (and l-buffer (buffer-live-p l-buffer))
;;	       (save-excursion
;;		 (set-buffer l-buffer)
;;		 (message "????: -%s-, modelinestring: -%s-" l-buffer vc-cmsyn-modeline-string)
;;		 (when (string-match "CMSynergyRetrievingVersion...$" vc-cmsyn-modeline-string )
;;		   (setq l-filename (file-name-nondirectory (buffer-file-name)))
;;		   (vc-cmsyn-int-update-modeline l-filename)
;;		   ;; ----------
;;		   ;; and remove the mapping from the (global) alist again
;;		   ;; ----------
;;		   (vc-cmsyn-clear-buffer-mapping-for-proc p-proc)
;;		   )
;;		 )
;;	       )
;;	     )
;;	   )
;;	 )
;;       )
;;      (set-process-filter
;;       l-proc
;;       (lambda (p-proc p-string)
;;	 ;; ----------
;;	 ;; put the info from CM Synergy in the mode-line
;;	 ;; ----------
;;	 (message "!updating modeline filter!, string: -%s-" p-string)
;;	 (save-match-data 
;;	   (let* 
;;	       (
;;		(l-ccm-version (if (string-match "\\([^\:]+:[^:\n]+\\)\n" p-string) (match-string 1 p-string) "CMSynergyVersionUnknown"))
;;		(l-buffer (vc-cmsyn-get-buffer-mapped-to-proc p-proc))
;;		l-filename
;;		)
;;	     (message "!! Popbuffer in filter: -%s-, ccm-version: -%s-" l-buffer l-ccm-version)
;;	     (when (and l-buffer (buffer-live-p l-buffer))
;;	       (save-excursion
;;		 (set-buffer l-buffer)
;;		 (setq l-filename (file-name-nondirectory (buffer-file-name)))
;;		 (vc-cmsyn-int-update-modeline (format "%s-%s" l-filename l-ccm-version))
;;		 )
;;	       )
;;	     ;; ----------
;;	     ;; and remove the mapping from the (global) alist again
;;	     ;; ----------
;;	     (vc-cmsyn-clear-buffer-mapping-for-proc p-proc)
;;	     )
;;	   )
;;	 )
;;       )
;;      )
;;    )
;;   (vc-cmsyn-check-logout)
;;  )

(provide 'vc-cmsyn)

;;; vc-cmsyn.el ends here
