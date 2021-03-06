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

;; This file defines the different constants. These are defined using
;; the defconst paradigm.

;;; Code:

;;;; Versioning.

(defconst vc-rational-synergy-int-version-string
  "1.0.1"
  "The version string of the current rational synergy integration module")

(defconst vc-rational-synergy-int-date-string
  "2015-06"
  "The date of this version")

(defconst vc-rational-synergy-int-author-string 
  "Sjoerd van Leent/Geert Ribbers/Henrik Joensson"
  "Author String of the IBM Rational Synergy module")

;;;; Authentication.

(defconst vc-rational-synergy-int-username-query-format
  "Enter Rational Synergy user name"
  "Input of username")

(defconst vc-rational-synergy-int-host-query-format
  "Enter Rational Synergy host name"
  "Input of host")

(defconst vc-rational-synergy-int-database-query-format
  "Enter Rational Synergy database name"
  "Input of database")

(defconst vc-rational-synergy-int-password-query-format
  "Enter Rational Synergy password"
  "Input of password")

;;;; Files.

(defconst vc-rational-synergy-int-file-name
  "File Name"
  "The file name")

(defconst vc-rational-synergy-int-type
  "File Type"
  "The file type")

(defconst vc-rational-synergy-int-rev
  "File version"
  "The revision of a file")

(defconst vc-rational-synergy-int-instance
  "File instance"
  "The instance number of a file")

(defconst vc-rational-synergy-int-status
  "Status"
  "The status of an object")

;;;; Properties.

(defconst vc-rational-synergy-int-property
  "Property"
  "The property")

(defconst vc-rational-synergy-int-value
  "Value"
  "The value")


(provide 'vc-rational-synergy-constants)

;;; vc-rational-synergy-constants.el ends here
