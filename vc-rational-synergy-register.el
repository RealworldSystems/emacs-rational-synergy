;;; vc-rational-synergy-base.el --- IBM Rational Synergy registration
 
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
;;     
;; Maintainer:
;;     Sjoerd van Leent <sjoerd.van.leent@realworld-systems.com>
;;
;; Homepage: http://github.com/RealworldSystems/emacs-rational-synergy
;;
;; Keywords: ibm rational synergy telelogic vc version-control


;;; Commentary:

;; Allows for registering file and directory objects into
;; IBM Rational Synergy.


;;; Code:

(require 'vc-rational-synergy-command-to-string)
(require 'vc-rational-synergy-utilities)


;;;; Internals.

;; The following functions aid in acquiring the type and version for a
;; newly added file or directory object

(defun vc-rational-synergy--acquire-type ()
  "If `vc-rational-synergy-query-create-file-type' is set, ask for type"
  (when vc-rational-synergy-query-create-file-type
    (let ((value (read-string "Type of the file (empty for default): ")))
      (cond ((eq value nil) nil)
	    ((string= value "") nil)
	    (t value)))))

(defun vc-rational-synergy--acquire-version()
  "If `vc-rational-synergy-query-create-file-version' is set, ask for version"
  (when vc-rational-synergy-query-create-file-version
    (let ((value (read-string "Version of the file (empty for default): ")))
      (cond ((eq value nil) nil)
	    ((string= value "") nil)
	    (t value)))))

;;;; CLI integration.

(defun vc-rational-synergy--command-register-file (file-name type)
  "Registers (adds) a file to the default task
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (let ((command-line (if type
			  `("create" "-type" ,type ,file-name)
			`("create" ,file-name))))
    (condition-case err
	(vc-rational-synergy-command-to-string command-line)
      (error nil))))

(defun vc-rational-synergy--command-set-version (file-name version)
  "Sets the version of the given file name
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (condition-case err
      (vc-rational-synergy-command-to-string
       `("attr" "-modify" "version" "-value" ,version ,file-name))
    (error nil)))

(defun vc-rational-synergy-register-and-set-version (file-name type version)
  "Combines the commands to register and set the version
Returns non-nil on success"
  (when (vc-rational-synergy--command-register-file file-name type)
    (if version
	(vc-rational-synergy--command-set-version
	 (buffer-file-name) version)
      t)))

;;;; User interaction.

;;;###autoload
(defun vc-rational-synergy-register-file ()
  "Registers the file visited by the buffer into IBM Rational Synergy
If configured to do as such, asks the user for type and version information"
  (interactive)
  (with-vc-rational-synergy
   (when (vc-rational-synergy--check-buffer-assoc)

     (let* ((type    (vc-rational-synergy--acquire-type))
	    (version (vc-rational-synergy--acquire-version)))
       (if (vc-rational-synergy-register-and-set-version (buffer-file-name)
							 type version)
	   (progn
	     (vc-rational-synergy-message "Registered file: %s"
					  (buffer-file-name))
	     (revert-buffer nil t))
	 (vc-rational-synergy-message "Did not register file: %s"
				      (buffer-file-name)))))))

(defun vc-rational-synergy--directory-files (directory-name)
  "Aquires all directory files in a given directory"
  ;; Upper directory levels should always be skipped, so when asking for
  ;; all files in the directory, the . and .. directories are to be
  ;; omitted (if the system returns them).
  ;;
  ;; Further, objects which are then valid, are to be tested against
  ;; the ignore listing.
  (let* ((objects (directory-files directory-name t "\\(^[^\\.]\\|^\\.[^.]\\)")))
    (delq nil (mapcar (lambda (o)
			(unless (vc-rational-synergy-ignore-test o)
			  (unless (string= (file-name-nondirectory o)
					   "_ccmwaid.inf") o)))
		      objects))))

(defun vc-rational-synergy--register-directory-rec
  (directory-name version &optional stop registered unregistered)
  "Aids in registering a directory and it's files recursively
DIRECTORY-NAME contains the actual name of the directory to register.
the VERSION can contain a positive number, indicating to set a version
higher than normal. if STOP is set, discontinue recursion.
Returns a pair with lists of simple files, the car is registered,
the cdr is not registered"
  
  ;; If the given directory-name matches against vc-rational-synergy-ignore-test,
  ;; or if directory is "." or ".." bail out.

  (when (vc-rational-synergy-ignore-test directory-name)
    (error (format "Directory %s can not be registered due to ignore settings"
		   directory-name)))
  (when (or (string= "." directory-name)
	    (string= ".." directory-name))
    (error "Parent- or self-reference is not allowed"))

  ;; Get all directory files, and split the files into directories and
  ;; real files.

  (let* ((objects (vc-rational-synergy--directory-files directory-name))
	 (directories (delq nil (mapcar (lambda (o)
					  (when (and (file-directory-p o)
						     (not (file-symlink-p o))) o))
					objects)))
	 (files (delq nil (mapcar (lambda (o)
					  (unless (or (file-directory-p o)
						      (file-symlink-p o)) o))
					objects))))
    
    ;; Attempt to register the directory itself
    (if (vc-rational-synergy-register-and-set-version directory-name
						      nil version)
	(setq registered (cons `(dir . ,directory-name) registered))
      (setq unregistered (cons `(dir . ,directory-name) unregistered)))
    
    ;; Register all files inside the directory
    (dolist (file files)
      (if (vc-rational-synergy-register-and-set-version file
							nil version)
	  (setq registered (cons `(file . ,file) registered))
	(setq unregistered (cons `(file . ,file) unregistered))))
    
    ;; Recusively register underlying directories, if stop is not set
    (unless stop
      (dolist (dir directories)
	(let ((res (vc-rational-synergy--register-directory-rec 
		    dir version stop registered unregistered)))
	  (setq registered (car res))
	  (setq unregistered (cdr res))))))
  `(,registered . ,unregistered))


(defun vc-rational-synergy-show-registered (registered not-registered)
  "Shows all files which have not been registered
SIMPLE-FILES is a list with a cons pair containing type (dir|file) and name"
  (let ((buffer (vc-rational-synergy-buffer)))
    (pop-to-buffer buffer)
    (erase-buffer)
    (when registered
      (insert (vc-rational-synergy--tabular-sf registered
					       "Could register:"))
      (insert "\n\n"))
    (when not-registered
      (insert (vc-rational-synergy--tabular-sf not-registered
					       "Could not register:"))
      (vc-rational-synergy-message "Could not register all files"))))


;;;###autoload
(defun vc-rational-synergy-register-directory ()
  "Registers the current directory recursively into IBM Rational Synergy
Typically, there are a good number of files which are artifacts of compilation
stages, in the case for C these would be object (o) files, in the case of Java
these would typically be java bytecode (javac) files, in the case of Emacs Lisp
these would be compiled emacs lisp (elc) files."
  (interactive)
  (condition-case err
      (with-vc-rational-synergy
       (let* ((version	     (vc-rational-synergy--acquire-version))
	      (directory-name    (vc-rational-synergy--buffer-directory))
	      (recursive	     (when directory-name
				       (y-or-n-p (format "Register %s recursively? "
							 directory-name))))
	      (result (vc-rational-synergy--register-directory-rec directory-name 
								   version 
								   (not recursive))))

	 (vc-rational-synergy-show-registered (car result) (cdr result))
	 (vc-rational-synergy--revert-buffers)))
    (error (vc-rational-synergy-message "%s" (error-message-string err)))))

(provide 'vc-rational-synergy-register)

;;; vc-rational-synergy-register.el ends here.
