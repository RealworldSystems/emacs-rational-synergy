;;; vc-rational-synergy-utilities.el --- IBM Rational Synergy integration for Emacs

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

;;;###autoload
(defun vc-rational-synergy-unixify-path (path)
  "Converts a platform specific representation of PATH to a Unix representation
without adding a drive prefix"
  (mapconcat '(lambda(x) (char-to-string (if (eq x ?\\) ?/ x))) path nil))

;;;###autoload
(defun vc-rational-synergy-platformify-path (path)
  "Converts a Unix representation of a path to a local platform representation
without adding a drive prefix"
  (if (eq system-type 'windows-nt)
      (mapconcat '(lambda(x) (char-to-string (if (eq x ?/) ?\\ x))) path nil)
    (mapconcat '(lambda(x) (char-to-string (if (eq x ?\\) ?/ x))) path nil)))

(provide 'vc-rational-synergy-utilities)

;; vc-rational-synergy-utilities.el ends here
