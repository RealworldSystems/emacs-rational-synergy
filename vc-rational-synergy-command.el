;;; vc-rational-synergy-command.el --- IBM Rational Synergy command execution

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

;;;; Customization definitions.

(require 'vc-rational-synergy-buffer)

(defgroup vc-rational-synergy-tooling nil
  "Tooling definition options for Rational Synergy"
  :tag "Tooling definition options for Rational Synergy"
  :group 'vc-rational-synergy)

(defcustom vc-rational-synergy-startup-path ""
  "Sets the working path, if any, of the synergy binary"
  :tag "Sets the working path, if any, of the synergy binary"
  :type 'string
  :group 'vc-rational-synergy-tooling)

(defcustom vc-rational-synergy-binary-name "ccm"
  "The Rational Synergy executable name"
  :tag "Rational Synergy executable name"
  :type 'string
  :group 'vc-rational-synergy-tooling)

(defun vc-rational-synergy-binary-name ()
  "Returns the binary name to be called for a particular executable.
Prefixes the binary name with the path given in
`vc-rational-synergy-startup-path' if applicable, otherwise returns directly
the expression of the customization value `vc-rational-synergy-binary-name'"
  (if (string= "" vc-rational-synergy-startup-path)
      vc-rational-synergy-binary-name
    (expand-file-name vc-rational-synergy-binary-name vc-rational-synergy-startup-path)))


(provide 'vc-rational-synergy-command)

;; vc-rational-synergy-command.el ends here
