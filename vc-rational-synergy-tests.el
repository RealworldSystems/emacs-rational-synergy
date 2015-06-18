;;; vc-rational-synergy-tests.el --- IBM Rational Synergy integration tests
 
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
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
 

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

;; These tests are interactive functional tests which can be used
;; to test certain capabilities. 
 
;;; Code:

(require 'vc-rational-synergy-authentication)

(defun vc-rational-synergy-test-session ()
  "Based on settings of customization, tests whether it is possible
to load a session without performing any actual commands. The result
of this test should be t"
  (interactive)
  
  (with-vc-rational-synergy
   (when t t)))
  

(provide 'vc-rational-synergy-tests)

;;; vc-rational-synergy-tests.el ends here
