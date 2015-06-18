;;; vc-rational-synergy-administration-customization.el --- IBM Rational Synergy admin customization
 
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

;; This file contains all the necessary administrative customization directives

;;; Code:

(require 'vc-rational-synergy-customization-base)

(defgroup vc-rational-synergy-admin nil
  "Administrative group for IBM Rational Synergy"
  :tag "Administrative group for IBM Rational Synergy"
  :group 'vc-rational-synergy
  )

(defcustom vc-rational-synergy-register-directory-and-files-filter
  (list "~$" ",v$" "#$")
  "user, pwd and synergy-database to use for auto-login"
  :tag "CM Synergy auto-login-data"
  :type '(repeat
	  (regexp :tag "Regexp for files that should not be registered with `Register directory + files'")
	  )
  :group 'vc-rational-synergy-admin)

(defcustom vc-rational-synergy-auto-check-in-directory-p
  nil
  "*Should vc-rational-synergy check-in the directory immediately again when a file is checked out in this dir?
With `Collaborative' personal work-areas the default Synergy-behaviour is to check out a directory when a file is checked out.
It may be better to keep the directories in integrate state so it's the same for everyone..
  Date          : Nov/2003
  Author        : Realworld Systems (GR)."
  :tag "Auto check-in directory on file-checkout?"
  :type 'boolean
  :group 'vc-rational-synergy-admin
  )

(defcustom vc-rational-synergy-check-default-task-set-p
  nil
  "*Boolean indicating if it has to be checked if a default task is set before performing a ccm update.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Check for default task set?"
  :type 'boolean
  :group 'vc-rational-synergy-admin
  )

(defcustom vc-rational-synergy-query-create-file-type
  t
  "*Boolean indicating if type of file has to be queried before registering a file in ccm.
Typically this will be organized within CM Synergy already so it goes automatically.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Query File-type?"
  :type 'boolean
  :group 'vc-rational-synergy-admin
  )

(defcustom vc-rational-synergy-query-create-file-version
  t
  "*Boolean indicating if version of file has to be queried before registering a file in ccm.
Typically this will be organized within CM Synergy already so it goes automatically.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Query File-version?"
  :type 'boolean
  :group 'vc-rational-synergy-admin
  )

(defcustom vc-rational-synergy-auto-check-status-file-open-p
  1
  "*Indicates if cmsynergy status should be checked for on opening of files.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Auto check status on file open?"
  :type 'boolean
  :set 'vc-rational-synergy-toggle-auto-check-status-file-open
  :group 'vc-rational-synergy-admin
  )

(defcustom vc-rational-synergy-buffer-name "*vc-rational-synergy*"
  "Name of buffer in which to log CM Synergy actions."
  :tag "CM Synergy output buffer name"
  :type 'string
  :group 'vc-rational-synergy-admin)

(defcustom vc-rational-synergy-version-object-separator
  "~"
  "The CM Synergy object-version separator, this is configured in CM Synergy, separates object-name and version in 4-part-names."
  :tag "Project Version Object Separator"
  :type '(character :tag "CM Synvergy Separator-char")
  :group 'vc-rational-synergy-admin)

(defcustom vc-rational-synergy-parallel-versions-string
  "\\(Warning: \\)?Parallel \\(versions\\|branches\\) exist for "
  "*String in CM Synergy output when a parallel version exists.
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  :tag "String output by CM Synergy when a parallel version exists"
  :type 'string
  :group 'vc-rational-synergy-admin
  )


(provide 'vc-rational-synergy-administration-customization)

;;; vc-rational-synergy-administration-customization ends here
