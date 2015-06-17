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


(require 'vc-rational-synergy-customization-base)

;;;; Customization definitions.

(defgroup vc-rational-synergy-tooling nil
  "Tooling definition options for Rational Synergy"
  :tag "Tooling definition options for Rational Synergy"
  :group 'vc-rational-synergy)

(defcustom vc-rational-synergy-startup-path ""
  "Sets the working path, if any, of the synergy binary"
  :tag "Sets the working path, if any, of the synergy binary"
  :type 'string
  :group 'vc-rational-synergy-tooling)

(defcustom vc-cmsyn-exe-name "ccm"
  "The Rational Synergy executable name"
  :tag "Rational Synergy executable name"
  :type 'string
  :group 'vc-rational-synergy-tooling)


;;;###autoload
(defun vc-rational-synergy-command-to-string (command-line)
  "Call `vc-cmsyn-exe-name' using one or more arguments
as it is illegal to call the binary without any arguments
a premature error is thrown when this attempt is being made

The command-line parameter should be a valid list containing
individual (expanded) strings. This command does not perform
any command-line expansion such as expanding wildcards.

If everything is successful, the command returns a string
with the actual content returned by the command"
  
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
  
  (let ((binary (if (string= "" vc-rational-synergy-startup-path)
		    vc-cmsyn-exe-name
		  (expand-file-name vc-cmsyn-exe-name vc-rational-synergy-startup-path))))
    (let ((lines (apply 'process-lines binary command-line))
	  (result nil))
      (message (prin1-to-string lines))
      (dolist (elt lines)
	(if result
	    (setq result (concat result (make-string 1 ?\n) elt))
	  (setq result elt)))
      result)))
    

(provide 'vc-rational-synergy-command-to-string)


;; vc-rational-synergy-command-to-string.el ends here
