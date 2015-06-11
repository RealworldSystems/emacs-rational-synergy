;;; vc-cmsyn-el --- IBM Rational Synergy integration for Emacs
 
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

;; This file defines methods to display information about the module.

;;; Code:

(require 'vc-rational-synergy-constants)

(defun vc-rational-synergy-integration-version ()
  "Display the version of CM Synergy Integration"
  (interactive)
  (message vc-rational-synergy-int-version-string))

(defun vc-rational-synergy-integration-date ()
  "Display the date of CM Synergy Integration"
  (interactive)
  (message vc-rational-synergy-int-date-string))

(defun vc-rational-synergy-integration-author ()
  "Display the author of CM Synergy Integration"
  (interactive)
  (message vc-rational-synergy-int-author-string))

;;;###autoload
(defun vc-rational-synergy-about ()
  "Display version, date, author of the IBM Rational Synergy integration"
  (interactive)
  (message-box "CM Synergy GNU Emacs Integration v%s, %s, %s"
		   vc-rational-synergy-int-version-string
		   vc-rational-synergy-int-date-string
		   vc-rational-synergy-int-author-string))

(provide 'vc-rational-synergy-about)

;;; vc-rational-synergy-about.el ends here
