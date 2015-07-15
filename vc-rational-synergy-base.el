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
(require 'vc-rational-synergy-command)
(require 'vc-rational-synergy-utilities)
(require 'vc-rational-synergy-authentication)
(require 'vc-rational-synergy-modeline)
(require 'vc-rational-synergy-project)
(require 'vc-rational-synergy-task)
(require 'vc-rational-synergy-checkin)
(require 'vc-rational-synergy-checkout)
(require 'vc-rational-synergy-register)
(require 'vc-rational-synergy-history)

(require 'vc-rational-synergy-administration-customization)
(require 'vc-rational-synergy-user-customization)


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


;;; HERE THINGS ARE REFACTORED

;;;; Properties of a file

(defun vc-rational-synergy--beautify-property (a-symbol)
  "Given a symbol, uppercases and eradicates the hyphen"
  (let* ((name (symbol-name a-symbol))
	 (splitted (split-string name "\\-")))
    (mapconcat 'capitalize splitted " ")))

(defun vc-rational-synergy--command-properties (file-name)
  "Acquire the properties of a given object
This is a wrapper around `vc-rational-synergy-command-w/format-to-list'"
  (condition-case err
      (let* ((order '(owner status create-time modify-time 
			   platform release created-in local-to task))
	     (result (car (apply 'vc-rational-synergy-command-w/format-to-list
				 `("prop" ,file-name) order)))
	     ret v)
	(when result
	  (dotimes (i (length order) ret)
	    (setq ret (cons `(,(vc-rational-synergy--beautify-property
				(nth i order)) . ,(nth i result)) ret)))))
    (error nil)))
  

;;;###autoload
(defun vc-rational-synergy-properties ()
  "Display the properties of the file in the work buffer."
  (interactive)
  (with-vc-rational-synergy
   (when (vc-rational-synergy--check-buffer-assoc)
     (let ((a-list (vc-rational-synergy--command-properties (buffer-file-name)))
	   (buffer (vc-rational-synergy-buffer))
	   (file-name (buffer-file-name)))
       (when a-list
	 (pop-to-buffer buffer)
	 (erase-buffer)
	 (insert (vc-rational-synergy--tabular-props a-list 
						     file-name))))))
  (message ""))


(provide 'vc-rational-synergy-base)

;;; vc-rational-synergy-base.el ends here
