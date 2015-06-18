;;; vc-rational-synergy-authentication-customization.el --- IBM Rational Synergy integration for Emacs
 
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

;; These are the customization options of the IBM Rational Synergy
;; authentication process. They dictate how and where logon and logoff
;; should be performed.

;;; Code:

(require 'vc-rational-synergy-customization-base)

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

(provide 'vc-rational-synergy-authentication-customization)

;;; vc-rational-synergy-authentication-customization.el ends here
