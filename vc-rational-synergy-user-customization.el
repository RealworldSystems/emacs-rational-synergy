;;; vc-rational-synergy-user-customization.el --- IBM Rational Synergy user customization
 
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

;; This file contains all the necessary user customization directives.

;;; Code:


(require 'vc-rational-synergy-customization-base)

(defcustom vc-rational-synergy-command-end-jump-to-output-buffer-p t
  "Jump to CM Synergy output buffer at end of CM Synergy commands?"
  :tag "Jump to Output-buffer After Commands?"
  :type 'boolean
  :group 'vc-rational-synergy)

(defcustom vc-rational-synergy-register-checks-in-p t
  "If this is non-nil registering a file into CM Synergy will check it in at the same time."
  :tag "Check a file in when registering?"
  :type 'boolean
  :group 'vc-rational-synergy)

(defcustom vc-rational-synergy-iconify-frame-when-ccm-gui
  t
  "*Boolean indicating if the current emacs-frame should be iconified when a ccm gui is called.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Iconify emacs when selecting tasks?"
  :type 'boolean
  :group 'vc-rational-synergy)

(provide 'vc-rational-synergy-user-customization)

;;; vc-rational-synergy-user-customization ends here
