;;; vc-rational-synergy-authentication-utilities.el --- IBM Rational Synergy integration for Emacs
 
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

;;; Commentary:

;; These are definitions used by the authentication process itself,
;; which are not part of authentication proper, but provide the means
;; to enable authentication and session maintenance.

;;; Code:

(require 'vc-rational-synergy-authentication-customization)
(require 'vc-rational-synergy-constants)
(require 'eieio)

;;;; Internals.

;; The following two function procedures enable a uniform way of quering
;; validity of a setting and reading a setting from the minibuffer, using
;; a modern "hit enter for default" strategy rather than having to erase
;; the contents of the query in the minibuffer to redefine the value

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


;;;; Proper accessors.

;; These accessors are quite a bit more enhanced, as they do not
;; only use customization to deal with acquiring the proper values
;; but provide a little bit more intelligence, enabling users of
;; the IBM Rational Synergy interface to use different means to
;; enable usage, such as provisioning through a shell script file or
;; log-in script.

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

;; These functions are the functions actually used to read the username,
;; password, database and host. Their values are preset if the previous
;; defaults are derived. The only value acting differently is the password
;; as it is deemed to dangerous to have a password preset.

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

;; The following provides a class which enables storage of username,
;; password, database and host.


(defclass vc-rational-synergy-authentication-params ()
  ((username :initarg :username)
   (password :initarg :password)
   (database :initarg :database)
   (host :initarg :host))
  "Instances of this class definition are used to pass the
parameters for authentication.")

(defmethod vc-rational-synergy-create-start-command-line 
  ((obj vc-rational-synergy-authentication-params))
  "Creates a start command line in the form of:	
* ccm start -d <database> -s <host> -u <username> -m -q -pw <password>"

  `("start"
    "-d" ,(oref obj database) 
    "-s" ,(oref obj host) 
    "-pw" ,(oref obj password)
    "-u" ,(oref obj username)
    "-m" "-q"))

(defun vc-rational-synergy-authentication-read ()
  "Reads information to be able to form an authentication object.
The authentication object will be used to extract the necessary information
to set-up a Rational Synergy session"
  (make-instance 'vc-rational-synergy-authentication-params
		 :username (vc-rational-synergy-authentication-read-username)
		 :password (vc-rational-synergy-authentication-read-password)
		 :database (vc-rational-synergy-authentication-read-database)
		 :host     (vc-rational-synergy-authentication-read-host)))



(provide 'vc-rational-synergy-authentication-utilities)

;;; vc-rational-synergy-authentication-utilities.el ends here
