;;; vc-rational-synergy-authentication.el --- IBM Rational Synergy integration for Emacs
 
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
;;     Geert Ribbers
;;     
;; Maintainer:
;;     Sjoerd van Leent <sjoerd.van.leent@realworld-systems.com>
;;
;; Homepage: http://github.com/RealworldSystems/emacs-rational-synergy
;;
;; Keywords: ibm rational synergy telelogic vc version-control

;;; Code:

(require 'vc-rational-synergy-authentication-utilities)
(require 'vc-rational-synergy-command-to-string)
(require 'eieio)


;;;; Raw internal session maintenance.

;; This section defines normal session maintenance semantics, the first
;; function is not-internal as vc-rational-synergy-session-exists is
;; harmless.

;; However, the other two functions are harmful, as they can offset
;; the behavior of the CCM tool and scramble the state of
;; the emacs integration facilities.

(defun vc-rational-synergy-session-exists ()
  "Checks if a rational synergy session exists"
  (when (getenv "CCM_ADDR") t))

(defun vc-rational-synergy-address ()
  "Returns the address on which CCM is running
Displays the address in the minibuffer if called interactively by the user"
  (interactive)
  (let ((env-setting (getenv "CCM_ADDR")))
    (when (called-interactively-p 'interactive)
      (message env-setting))
    env-setting))



;; TODO: In the future, these might be changed into lambdas with a
;;       let concept around this entire module.


(defun vc-rational-synergy--raw-start-session (authentication-params)
  "Performs a raw startup of a session, and sets CCM_ADDR"
  (let ((result (vc-rational-synergy-command-to-string
		 (vc-rational-synergy-create-start-command-line authentication-params))))
    (setenv "CCM_ADDR" result)))
	
(defun vc-rational-synergy--raw-kill-session ()
  "Performs a raw kill, if CCM_ADDR is set"
  (when (vc-rational-synergy-session-exists)
    (let ((result (vc-rational-synergy-command-to-string '("stop"))))
      (setenv "CCM_ADDR" nil)
      result)))
	

;;;; Provisioning.

;; Provisioning is the set up of how session maintenance is performed,
;; based on a few rules:

;; If vc...authentication-explicit is defined as not-nil,
;; it is illegal for the vc...check-session function to spawn an
;; explicit provisioner.
;;
;; If vc...authentication-license-friendly is defines as not-nil,
;; the license friendly provisioners are to be used instead of the
;; session guarding provisioners. They are however significantly
;; less efficient.

(defclass vc-rational-synergy-provisioner ()
  ((authentication-params :initarg :params :initform nil))
  "Abstract provisioner, which has two methods:
* vc-rational-synergy-provisioner-setup (default, nothing)
* vc-rational-synergy-provisioner-start (abstract)
* vc-rational-synergy-provisioner-pause (default, nothing)
* vc-rational-synergy-provisioner-teardown"
  :abstract t)

(defvar vc-rational-synergy--current-provisioner nil
  "When a provisioner starts, this value is set. Only when a 
provisioner ends, another (type of) provisioner could take it's place")

(defmethod vc-rational-synergy-provisioner--authenticate
  ((obj vc-rational-synergy-provisioner))
  "Performs direct authentication, by checking whether a session exists.
If not so, the session will be created by first initialzing authentication,
if this is not performed yet. Once authentication is set, the authentication
parameters will be stored and reused when necessary"
  (message "Checking if session is available")
  (unless (vc-rational-synergy-session-exists)
    ;; Should startup a synergy session
    (unless (oref-default obj :params)
      (oset obj :params (vc-rational-synergy-authentication-read)))
    (message "Attempt authentication")
    ;; Start the session
    (vc-rational-synergy--raw-start-session (oref obj :params))
    (message (concat "Authentication successful using session: "
		     (vc-rational-synergy-address)))))

(defmethod vc-rational-synergy-provisioner-teardown
  ((obj vc-rational-synergy-provisioner))
  "Tears down an existing instance. Typically, this invokes the pause method,
which for license friendly provisioners should terminate the session"
  
  (vc-rational-synergy-provisioner-pause obj))

(defmethod vc-rational-synergy-provisioner-pause
  ((obj vc-rational-synergy-provisioner))
  "Stops provisioning, with option to resume provisioning. By default,
this does not do anything"
  t)

(defmethod vc-rational-synergy-provisioner-setup
  ((obj vc-rational-synergy-provisioner))
  "Sets up provisioning. By default, this does not do anything but acts
as stub"
  t)  

;; Automatic provisioning.
(defclass vc-rational-synergy-implicit-provisioner (vc-rational-synergy-provisioner)
  ()
  "Starts an implicit provisioner, allowing for entering the credentials
on demand")

(defmethod vc-rational-synergy-provisioner-start 
  ((obj vc-rational-synergy-implicit-provisioner))
  "Starts provisioning a proper instance of a particular session"
  (vc-rational-synergy-provisioner--authenticate obj))

(defmethod vc-rational-synergy-provisioner-teardown :after
  ((obj vc-rational-synergy-implicit-provisioner))
  "Executes this method after normal execution of teardown by the abstract
provisioner"
  (vc-rational-synergy--raw-kill-session))


;; Automatic license friendly provisioning.
(defclass vc-rational-synergy-implicit-friendly-provisioner (vc-rational-synergy-provisioner)
  ()
  "Starts an implicit license friendly provisioner,
allowing for entering the credentials on demand")

(defmethod vc-rational-synergy-provisioner-start 
  ((obj vc-rational-synergy-implicit-friendly-provisioner))
  "Starts provisioning a proper instance of a particular session"
  (vc-rational-synergy-provisioner--authenticate obj))

(defmethod vc-rational-synergy-provisioner-pause 
  ((obj vc-rational-synergy-implicit-friendly-provisioner))
  "Pauses the current session instance"
  (vc-rational-synergy--raw-kill-session))



;; Explicit provisioning.
(defclass vc-rational-synergy-explicit-provisioner (vc-rational-synergy-provisioner)
  ()
  "Starts an explicit provisioner, allowing for entering the credentials
on demand")

(defmethod vc-rational-synergy-provisioner-setup
  ((obj vc-rational-synergy-explicit-provisioner))
  (vc-rational-synergy-provisioner--authenticate obj))

(defmethod vc-rational-synergy-provisioner-start 
  ((obj vc-rational-synergy-explicit-provisioner))
  "Starts provisioning a proper instance of a particular session"
  
  ;; A session must exist, otherwise this provisioner bails out
  (unless (vc-rational-synergy-session-exists)
    (error "Should explicitly log in to the session")))

(defmethod vc-rational-synergy-provisioner-teardown :after
  ((obj vc-rational-synergy-explicit-provisioner))
  "Executes this method after normal execution of teardown by the abstract
provisioner"
  (vc-rational-synergy--raw-kill-session))

;; Explicit, license friendly provisioning.
(defclass vc-rational-synergy-explicit-friendly-provisioner (vc-rational-synergy-provisioner)
  ()
  "Starts an explicit provisioner, allowing for entering the credentials
on demand, but closing the session after each pause")

(defmethod vc-rational-synergy-provisioner-setup
  ((obj vc-rational-synergy-explicit-friendly-provisioner))
  (vc-rational-synergy-provisioner--authenticate obj))

(defmethod vc-rational-synergy-provisioner-start 
  ((obj vc-rational-synergy-explicit-friendly-provisioner))
  "Starts provisioning a proper instance of a particular session"
  
  ;; Parameters should be set
  (unless (oref obj :params)
    (error "Should explicitly log in to the session"))

  (unless (vc-rational-synergy-session-exists)
    (vc-rational-synergy-provisioner--authenticate obj)))

(defmethod vc-rational-synergy-provisioner-pause 
  ((obj vc-rational-synergy-explicit-friendly-provisioner))
  "Pauses the current session instance"
  (vc-rational-synergy--raw-kill-session))
  

;;;; Session maintenance.

;; The following matrix allocates provisioners by dual keys


(defvar vc-rational-synergy--provisioning-matrix
  (make-hash-table :test 'equal)
  "This matrix contains the different provision strategies")

(let ((matrix vc-rational-synergy--provisioning-matrix))
  (puthash '(:explicit . :normal)
	   'vc-rational-synergy-explicit-provisioner matrix)
  (puthash '(:explicit . :friendly)
	   'vc-rational-synergy-explicit-friendly-provisioner matrix)
  (puthash '(:implicit . :normal)
	   'vc-rational-synergy-implicit-provisioner matrix)
  (puthash '(:implicit . :friendly)
	   'vc-rational-synergy-implicit-friendly-provisioner matrix))


(defun vc-rational-synergy--provision (mode)
  "Provisions using a certain given mode (:explicit or :implicit)"

  (let* (
	 ;; Runtype is either :friendly or :normal
	 (runtype    (if vc-rational-synergy-authentication-license-friendly
			 :friendly
		       :normal))
	 ;; Get the proper class-name out of the matrix
	 (class-name (gethash `(,mode . ,runtype)
			    vc-rational-synergy--provisioning-matrix))
	 
	 ;; Instantiate a provisioner
	 (instance (make-instance class-name)))
    
    ;; Set up the privisioner
    (vc-rational-synergy-provisioner-setup instance)

    ;; Set the current provisioner
    (setq vc-rational-synergy--current-provisioner instance)

    ;; Return the provisioner
    instance))


;;;###autoload
(defun vc-rational-synergy-check-session ()
  "This function checks if a session is avialable, based on the session
provisioning strategy. A new provisioning strategy is set up, as the once
auto-login-logout idea is unsafe and does not have all necessary data,
likewise, the manual login is defunct because GUI has become obsolete

There are two provisioning options: log-in explicitly, and log-in
automatically. The eventual methodology of logging in is similar."

  ;; If a provisioner is not present, check if we are allowed to use
  ;; automatic implicit provisioning. If so, initialize an implicit
  ;; provisioner

  (unless vc-rational-synergy--current-provisioner
    (when vc-rational-synergy-authentication-explicit
      (error "You are not allowed to use implicit authentication"))
    
    (vc-rational-synergy--provision :implicit))

  (vc-rational-synergy-provisioner-start vc-rational-synergy--current-provisioner))

;;;###autoload
(defun vc-rational-synergy-check-session-pause ()
  "Pauses the current session, if the provisioner implements this and
a provisioner is available"

  (when vc-rational-synergy--current-provisioner
    (vc-rational-synergy-provisioner-pause vc-rational-synergy--current-provisioner)))

    

;;;###autoload
(defun vc-rational-synergy-logout ()
  "Checks if a provisioner is running. If that is the case, 
terminate the provisioner"
  (interactive)


  (when vc-rational-synergy--current-provisioner
    (vc-rational-synergy-provisioner-teardown
     vc-rational-synergy--current-provisioner)
    (setq vc-rational-synergy--current-provisioner nil)))


;;;###autoload
(defun vc-rational-synergy-login ()
  "Starts an explicit CCM session, if no session is currently running"
  (interactive)

  ;; If a CCM session is started this way, it is always an explicit session,
  ;; even though it is allowed for a session to be started implicitly.
  ;;
  ;; Note that a provisioner is only started if there is not provisioner
  ;; available.
  
  (unless vc-rational-synergy--current-provisioner
    (vc-rational-synergy--provision :explicit)))

;;;###autoload
(defun vc-rational-synergy-logged-on-user ()
  "If a user is logged on, display the user which has logged on"
  (interactive)

  (let* ((status (vc-rational-synergy-status))
	 (user (when status (elt status 0))))
    (message (if user
		 (format "Currently logged on user: %s" user)
	       "No user logged on currently"))
    user))

;;;###autoload
(defun vc-rational-synergy-status ()
  "If a user is logged on, extract the status.
If called interactively, format the status such that it can be
displayed in the minibuffer

The returning status will be zero, or a vector of five values,
which are in order:
 1) username
 2) database
 3) address"
  (interactive)
  
  ;; Typically, a status results in a number of lines having the following
  ;; Mark-up:
  ;;
  ;; Line 1 contains a header
  ;; Line 2 is empty
  ;; Line 3 Contains the actual data
  ;; Line 4 Contains indication of project

  ;; The column specification is
  ;; 1) The user logged in
  ;; 2) The database connected to
  ;; 3) The session address
  ;; 4) The session type (is always CLI type)
  ;; 5) Whether or not there is a current session

  (let (actual-result
	(report "No session bound to Emacs")
	(interactively (called-interactively-p 'interactive)))

    ;; Only execute when provisioning, otherwise,
    (when vc-rational-synergy--current-provisioner
      (let ((result (vc-rational-synergy-command-w/format-to-list
		     '("status" "-cli")
		     'username 'database 'address 'session 'current-session)))
	(when (<= 1 (length result))
	  (dolist (r result)
	    (when (string= "TRUE" (nth 4 r))
	      (setq actual-result
		    (vector (nth 0 r)
			    (nth 1 r)
			    (nth 2 r)))))
	  (when interactively
	    (setq report (format "You are %s, on database [%s], on address [%s]"
				 (elt actual-result 0)
				 (elt actual-result 1)
				 (elt actual-result 2)))))))
    (when interactively (message report))
    actual-result))
    

;;;###autoload
(defmacro with-vc-rational-synergy (&rest program)
  "Checks session existence and maintain the session
This will call `vc-rational-synergy-check-session', protect the program and on
local or non-local exit (errors, etc.) run
`vc-rational-synergy-check-session-pause'

Called like this:

(with-vc-rational-synergy
  ...)

Expands to:

(progn (vc-rational-synergy-check-session)
       (unwind-protect
	   ...
	 (vc-rational-synergy-check-session)))"
  `(progn (vc-rational-synergy-check-session)
	  (unwind-protect
	      ,(cons 'progn program)
	    (vc-rational-synergy-check-session-pause))))


(provide 'vc-rational-synergy-authentication)

;;; vc-rational-synergy-authentication.el ends here
