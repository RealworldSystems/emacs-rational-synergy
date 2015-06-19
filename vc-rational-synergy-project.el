;;; vc-rational-synergy-project.el --- IBM Rational Synergy project commands
 
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
(require 'vc-rational-synergy-authentication)

;;;###autoload
(defun vc-rational-synergy-current-project ()
  "Identifies if there is a project, and if so, the details"
  (interactive)
  
  (with-vc-rational-synergy
   (let ((project-spec (vc-rational-synergy-command-w/format-to-list
			'("project")
			'project 'version)))
     (if project-spec
	 (let* ((project-name (nth 0 (car project-spec)))
		(project-version (nth 1 (car project-spec))))
	   (when (called-interactively-p 'interactive)
	     (message "Your project: [%s] Version [%s]" 
		      project-name project-version))
	   (vector project-name project-version))
       (when (called-interactively-p 'interactive)
	 (message "No project has been defined here")
	 nil)))))


(provide 'vc-rational-synergy-project)

;; vc-rational-synergy-current-project ends here
