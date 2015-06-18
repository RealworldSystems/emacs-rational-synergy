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


;;; Commentary:

;; These are utility functions used by the rational synergy integration

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

;;;###autoload
(defun vc-rational-synergy-dos2unix ()
 "Convert this buffer from <return><newline> endings to <newline> endings"
 (interactive)
 (save-excursion
   (goto-char (point-min))
   (while (re-search-forward "\r$" nil t)
     (replace-match "" nil nil))
   (goto-char (1- (point-max)))
   (if (looking-at "\C-z")
       (delete-char 1))))

(defun vc-rational-synergy-result-error-p (value)
  "Checks if a given result string indicates a CCM error"
  (string-match vc-rational-synergy-warning-error-output-regexp value))

(defun vc-rational-synergy-result-okay-p (value)
  "Checks if a given result string does not indicate a CCM error"
  (not vc-rational-synergy-result-error-p value))


(defmacro with-vc-rational-synergy-comint-strip-ctrl-m (&rest program)
  "Sets the shell-string-ctrl-m function to comint-output-filter-functions
and reverts back to the original after running program"
  `(progn
     (let ((old-output-filter-functions comint-output-filter-functions))
       (setq comint-output-filter-functions
	     (push 'shell-strip-ctrl-m old-output-filter-functions))
       (unwind-protect
	   ,(cons 'progn program)
	 (setq comint-output-filter-functions old-output-filter-functions)))))
	 
  


(provide 'vc-rational-synergy-utilities)

;; vc-rational-synergy-utilities.el ends here
