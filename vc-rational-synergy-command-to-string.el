;;; vc-rational-synergy-command-to-string.el --- IBM Rational Synergy integration for Emacs

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


;;; Code:


(require 'vc-rational-synergy-utilities)
(require 'vc-rational-synergy-command)


(defun vc-rational-synergy-command (command-line)
  "Call `vc-rational-synergy-binary-name' using one or more arguments
as it is illegal to call the binary without any arguments
a premature error is thrown when this attempt is being made

The command-line parameter should be a valid list containing
individual (expanded) strings. This command does not perform
any command-line expansion such as expanding wildcards.

If everything is successful, the command returns a list with lines containing
the actual content returned by the command"
  
  ;; The vc-cmsyn-command-to-string used to place the
  ;; commands into a temporary file. This usage has become
  ;; obsolete and disallowed.

  ;; Check to see if command-line is a list, contains only
  ;; string values and has length > 0
  (when (nlistp command-line)
    (error "command-line not a list"))

  (when (eq nil command-line)
    (error "command-line empty"))

  (dolist (elt command-line)
    (when (not (stringp elt))
      (error "command-line contains illegal values")))

  ;; Perform the command and construct s string from the lines
  ;; produced by process-lines. Return that string
  
  (condition-case err
      (progn
	(let ((lines (apply 'process-lines 
			    (vc-rational-synergy-binary-name) ;; name of binary
			    command-line)))
	  
	  (message (prin1-to-string lines))
	  lines))
    (error 
     (error "Could not execute CCM command, cause: [%s]"
	    (error-message-string err)))))


;;;###autoload
(defun vc-rational-synergy-command-w/format (command-line &rest format)
  "Runs a CCM command using a specific format.
The FORMAT is represented as a list of format keywords, and translated
to a reinterpretable format string from the result of
`vc-rational-synergy-command'.

represented in format (a list) to the command. COMMAND-LINE should begin
with the action identifier. The COMMAND-LINE should not include the
format options"
  (let* ((output-format (apply 'vc-rational-synergy--create-format-string
			       format))
	 (extended-command-line (append command-line
					`("-nch" "-f" ,output-format))))
    (vc-rational-synergy-command extended-command-line)))

;;;###autoload
(defun vc-rational-synergy-command-w/format-to-list (command-line &rest format)
  "Runs a CCM command using a specific format, returning extracted information
Does the same as `vc-rational-synergy-command-w/format', but returns not
the lines from the command but rather parses the output and sends it back
using `vc-rational-synergy--parse-formatted"
  (condition-case err
      (let ((result (apply 'vc-rational-synergy-command-w/format
			   command-line
			   format)))
	(vc-rational-synergy--parse-formatted result))
    (error (progn
	     (message (prin1-to-string err))
	     nil))))

;;;###autoload
(defun vc-rational-synergy-command-to-string (command-line)
  "Call `vc-rational-synergy-binary-name' using one or more arguments
as it is illegal to call the binary without any arguments
a premature error is thrown when this attempt is being made

The command-line parameter should be a valid list containing
individual (expanded) strings. This command does not perform
any command-line expansion such as expanding wildcards.

If everything is successful, the command returns a string
with the actual content returned by the command"

  (let ((lines (vc-rational-synergy-command command-line))
	(result nil))
    (dolist (elt lines)
      (if result
	  (setq result (concat result (make-string 1 ?\n) elt))
	(setq result elt)))
    (message result)
    result))
 

(provide 'vc-rational-synergy-command-to-string)


;; vc-rational-synergy-command-to-string.el ends here
