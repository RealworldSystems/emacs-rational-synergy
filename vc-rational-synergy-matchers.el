;;; vc-rational-synergy-matchers.el --- IBM Rational Synergy regular expressions
 
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

;; This require contains `vc-rational-synergy-version-object-separator'
(require 'vc-rational-synergy-administration-customization)

(defvar vc-rational-synergy-object-name-regexp
  (format "[^%s]+%s\\([^:]+\\):[^:]+:[^:]+" 
	  vc-rational-synergy-version-object-separator
	  vc-rational-synergy-version-object-separator)
  "*Regexp to match an object-name of CM Synergy.")

(defvar vc-rational-synergy-warning-error-output-regexp
  "\\`\\(Warning\\|Error\\):"
  "*Regexp to match waring-error output from CM Synergy.")

(defun vc-rational-synergy-output-is-warning-or-error (value)
  "Checks whether the given value might be a warning or error
Uses the `vc-rational-synergy-warning-error-output-regexp' to
scan whether or not the value given might be an error"
  (string-match vc-rational-synergy-warning-error-output-regexp
		value))


;;;; FIXME: None of these appear to be in use

(defvar vc-rational-synergy-object-name-list-output-regexp
  (format "^[0-9]+\)[ \t]+%s+" vc-rational-synergy-object-name-regexp)
;;   (format "^[0-9]+\)[ \t]+[^%s]+%s\\([^:]+\\):[^:]+:[^:]+" vc-rational-synergy-version-object-separator vc-rational-synergy-version-object-separator)
  "*Regexp to match output from ccm that should be a list with objectnames.")

(defvar vc-rational-synergy-file-created-output-regexp
  "[Mm]ember[ \t]*\\(.*\\)[ \t]*added to project[ \t]+\\([^ \t\n]+\\)[ \t]*$"
  "*Regexp to match correct file-creation output from CM Synergy.
  subexp 1 is fileleafname<`vc-rational-synergy-version-object-separator'>version
  subexp 2 is project-name")

(defvar vc-rational-synergy-file-ci-output-regexp
  "Checked in[ \t]*'\\([^']+\\)'[ \t]*to[ \t]*'\\([^']+\\)'"
  "*Regexp to match correct file-ci output from CM Synergy.
  subexp 1 is fileleafname<`vc-rational-synergy-version-object-separator'>version
  subexp 2 is new status")




(provide 'vc-rational-synergy-matchers)

;;; vc-rational-synergy-matchers.el ends here
