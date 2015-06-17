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

(require 'vc-rational-synergy-customization-base)
(require 'vc-rational-synergy-constants)
(require 'vc-rational-synergy-command-to-string)
(require 'eieio)

;;;; Internals.
(defun vc-rational-synergy--is-empty-setting (arg)
  (cond ((eq arg nil) t)
	((string= arg "") t)
	(t 'nil)))


(defun vc-rational-synergy--enhanced-read (display-string initial)
  ;; Reads from the minibuffer a string. If the string is empty, revert
  ;; to initial string.
  
  (let* ((echo-string (format "%s (Default [%s]): " display-string initial))
	 (value (read-string echo-string)))
    (if (vc-rational-synergy--is-empty-setting value)
	initial
      value)))


;;;; Customization definitions.

(defgroup vc-rational-synergy-authentication nil
  "Authentication provisioning options for Rational Synergy"
  :tag "Authentication provisioning options for Rational Synergy"
  :group 'vc-rational-synergy)


(defcustom vc-rational-synergy-authentication-explicit nil
  "If t, when a session is not available during a session command,
a session will not be attempted to be created. By default, a session
will be provisioned if no session is available"
  :tag "Authenticate explicitly"
  :type 'boolean
  :group 'vc-rational-synergy-authentication)

(defcustom vc-rational-synergy-authentication-license-friendly nil
  "If t, automatic reconnection is executed for every CCM operation"
  :tag "Reconnect for each individual CCM operation"
  :type 'boolean
  :group 'vc-rational-synergy-authentication)


(defcustom vc-rational-synergy-authentication-settings '("" "")
  "Database and host settings"
  :tag "Version database and host"
  :type '(list
	  (string :tag "Rational Synergy database specification")
	  (string :tag "Rational Synergy host specification"))
  :group 'vc-rational-synergy-authentication)

(defcustom vc-rational-synergy-authentication-username ""
  "Username override, if platform user is not to be used"
  :tag "Username override, if platform user is not to be used"
  :type 'string
  :group 'vc-rational-synergy-authentication)


;;;; Proper accessors.
  

(defun vc-rational-synergy-authentication-default-database ()
  "The default database is deduced through two ways: First, the
authentication settings are checked. If not database is specified,
the platform environment is scanned for the setting: CCM_DATABASE.
If neither can be found, nil is returned"
  (let ((db (car vc-rational-synergy-authentication-settings)))
    (if (vc-rational-synergy--is-empty-setting db)
	(getenv "CCM_DATABASE")
      db)))


(defun vc-rational-synergy--deduce-host-from-default-database ()
  "Deduces the host name from a database name by working out some
magic. This makes it possible to have only a database name in the
customization. It only works (of course) if a database name can
be deduced."
  (let ((db (vc-rational-synergy-authentication-default-database)))
    (when db
      (let* ((start (string-match "[[:word:]]+" db))
	     (end (match-end 0)))
	(concat "http://" (substring db start end) ":8500")))))
  

(defun vc-rational-synergy-authentication-default-host ()
  "The host is deduced through three ways. First, the authentication
settings are checked. If no host is defined, then the platform
environment is scanned for the setting: CCM_HOST. If CCM_HOST is
not found, the default-database setting is used, which is splitted
by non-alphanumeric characters, the first result then is used as
host name, appended by the default port of 8500"
  (let ((host (car (cdr vc-rational-synergy-authentication-settings)))
	(env-setting (getenv "CCM_HOST")))
    (cond ((not (vc-rational-synergy--is-empty-setting host)) host)
	  (env-setting env-setting)
	  (t (vc-rational-synergy--deduce-host-from-default-database)))))

(defun vc-rational-synergy-authentication-default-username ()
  "The user name is deduced through three ways. First, the authentication
username override is checked. If this is not set (or empty) the CCM_USERNAME
is checked. If that is not set, the system username is used. At the end, some
user name is returned"
  (let* ((username vc-rational-synergy-authentication-username)
	 (env-setting (getenv "CCM_USERNAME")))
    (cond ((not (vc-rational-synergy--is-empty-setting username)) username)
	  (env-setting env-setting)
	  (t (user-login-name)))))

;;;; Modeline queries.

(defun vc-rational-synergy-authentication-read-username ()
  "Reads the username from the minibuffer, using the default username if
none given"
  (vc-rational-synergy--enhanced-read
   vc-rational-synergy-int-username-query-format
   (vc-rational-synergy-authentication-default-username)))

(defun vc-rational-synergy-authentication-read-password ()
  "Reads the password from the minibuffer. If no password is given,
causes an error"
  (let* ((prompt vc-rational-synergy-int-password-query-format)
	 (password (read-passwd (format "%s: " prompt))))
    (when (string= password "") (error "Password must be given"))
    password))
	 
(defun vc-rational-synergy-authentication-read-database ()
  "Reads the database from the minibuffer, using the default database if
none given"
  (vc-rational-synergy--enhanced-read
   vc-rational-synergy-int-database-query-format
   (vc-rational-synergy-authentication-default-database)))

(defun vc-rational-synergy-authentication-read-host ()
  "Reads the host from the minibuffer, using the default host if
none given"
  (vc-rational-synergy--enhanced-read
   vc-rational-synergy-int-host-query-format
   (vc-rational-synergy-authentication-default-host)))
  
    
;;;; Authentication set up.


(defclass vc-rational-synergy-authentication-params ()
  ((username :initarg :username)
   (password :initarg :password)
   (database :initarg :database)
   (host     :initarg :host))
  "Instances of this class definition are used to pass the
parameters for authentication.")

(defmethod vc-rational-synergy-create-start-command-line 
  ((obj vc-rational-synergy-authentication-params))
  "Creates a start command line in the form of:	
* ccm start -d <database> -s <host> -u <username> -m -q -pw <password>"

  (with-slots (username password database host) obj
    `("start" "-d" ,database "-s" ,host "-u" ,username "-m" "-q" "-pw" ,password)))

(defun vc-rational-synergy-authentication-read ()
  "Reads information to be able to form an authentication object.
The authentication object will be used to extract the necessary information
to set-up a Rational Synergy session"
  (make-instance 'vc-rational-synergy-authentication-params
		 :username (vc-rational-synergy-authentication-read-username)
		 :password (vc-rational-synergy-authentication-read-password)
		 :database (vc-rational-synergy-authentication-read-database)
		 :host     (vc-rational-synergy-authentication-read-host)))


;;;; Session maintenance.

(defun vc-rational-synergy-session-exists ()
  "Checks if a rational synergy session exists"
  (when (getenv "CCM_ADDR") t))

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

(defclass vc-rational-synergy-provisioner ()
  ((authentication-params :initarg nil))
  "Abstract provisioner, which has two methods:
* vc-rational-synergy-provisioner-setup (default, nothing)
* vc-rational-synergy-provisioner-start (abstract)
* vc-rational-synergy-provisioner-pause (default, nothing)
* vc-rational-synergy-provisioner-teardown")

(defvar vc-rational-synergy--current-provisioner nil
  "When a provisioner starts, this value is set. Only when a 
provisioner ends, another (type of) provisioner could take it's place")

(defmethod vc-rational-synergy-provisioner--authenticate
  ((obj vc-rational-synergy-provisioner))
  "Performs direct authentication"
  (unless (vc-rational-synergy-session-exists)
    ;; Should startup a synergy session
    (unless (slot-value obj :authentication-params)
      (set-slot-value obj :authentication-params vc-rational-synergy-authentication-read))
    
    ;; Start the session
    (vc-rational-synergy--raw-start-session (slot-value obj :authentication-params))))

(defmethod vc-rational-synergy-provisioner-teardown
  ((obj vc-rational-synergy-provisioner))
  "Tears down an existing instance"
  
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

(defun vc-rational-synergy--provision (class-name)
  "Provisions using a certain given class name"
  (let ((instance (make-instance class-name)))
    (vc-rational-synergy-provisioner-setup instance)
    (setq vc-rational-synergy--current-provisioner instance)
    instance))
  

;;;; Automatic provisioning.
(defclass vc-rational-synergy-implicit-provisioner (vc-rational-synergy-provisioner)
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


;;;; Automatic license friendly provisioning.
(defclass vc-rational-synergy-friendly-provisioner (vc-rational-synergy-provisioner)
  "Starts an implicit license friendly provisioner,
allowing for entering the credentials on demand")

(defmethod vc-rational-synergy-provisioner-start 
  ((obj vc-rational-synergy-friendly-provisioner))
  "Starts provisioning a proper instance of a particular session"
  (vc-rational-synergy-provisioner--authenticate obj))

(defmethod vc-rational-synergy-provisioner-pause 
  ((obj vc-rational-synergy-friendly-provisioner))
  "Pauses the current session instance"
  (vc-rational-synergy--raw-kill-session))



;;;; Explicit provisioning.
(defclass vc-rational-synergy-explicit-provisioner (vc-rational-synergy-provisioner)
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
    (error "Should explicitly log in to the session")

(defmethod vc-rational-synergy-provisioner-teardown :after
  ((obj vc-rational-synergy-explicit-provisioner))
  "Executes this method after normal execution of teardown by the abstract
provisioner"
  (vc-rational-synergy--raw-kill-session))
  


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
    
    (if vc-rational-synergy-authentication-license-friendly
	(vc-rational-synergy--provision 'vc-rational-synergy-friendly-provisioner)
      (vc-rational-synergy--provision 'vc-rational-synergy-implicit-provisioner)))

  (vc-rational-synergy-provisioner-start vc-rational-synergy--current-provisioner))

;;;###autoload
(defun vc-rational-synergy-check-session-pause ()
  "Pauses the current session, if the provisioner implements this and
a provisioner is available"

  (when vc-rational-synergy--current-provisioner
    (vc-rational-synergy-provisioner-pause vc-rational-synergy--current-provisioner)))

    

;;;###autoload
(defun vc-cmsyn-check-logout ()
  "Checks if a provisioner is running. If that is the case, 
terminate the provisioner"
  
  (when vc-rational-synergy--current-provisioner
    (vc-rational-synergy-provisioner-teardown
     vc-rational-synergy--current-provisioner)
    (setq vc-rational-synergy--current-provisioner nil)))


;;;###autoload
(defun vc-cmsyn-start ()
  "Starts an explicit CCM session"
  (interactive)

  ;; If a CCM session is started this way, it is always an explicit session,
  ;; even though it is allowed for a session to be started implicitly.
  
  (unless vc-rational-synergy--current-provisioner
    (vc-rational-synergy--provision 'vc-rational-synergy-explicit-provisioner)))
