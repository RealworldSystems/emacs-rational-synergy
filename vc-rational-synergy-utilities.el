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

(defvar vc-rational-synergy--format-string-prefix "\\t>>>\\t"
  "Prefix of the format string send towards IBM Rational Synergy")

(defvar vc-rational-synergy--format-string-prefix-regexp "^\t>>>\t"
  "Prefix of the format string send back from IBM Rational Synergy")


;;;; I/O.

(defun vc-rational-synergy--create-format-string (&rest names)
  "Creates an understandable format string against IBM Rational Synergy
The names are to be symbols or strings specifying the format
names of a particular option.

Names with hyphens are automatically replaced with names using 
underscores"
  (let* ((proper-names (mapcar (lambda (n)
				 (cond 
				  ((symbolp n) (symbol-name n))
				  ((stringp n) n)
				  (t (error "not a string or symbol: %s" n))))
			       names))
	 (underscored (mapcar (lambda (n) (sub n "-" "_")) proper-names))
	 (body (mapconcat (lambda (n) (format "%%%s" n)) underscored "\\t")))
    (format "%s%s" vc-rational-synergy--format-string-prefix body)))

(defun vc-rational-synergy--parse-formatted-line (line)
  "Parses a single line.
If the line is valid to be parsed, returns a list containing the lines.
If not, the result will be nil"

  (when (stringp line)
    (when (eq 0 (string-match vc-rational-synergy--format-string-prefix-regexp
			      line))
	(let ((body (substring line (match-end 0))))
	  (split-string body "\t")))))
	  
		   

(defun vc-rational-synergy--parse-formatted (lines)
  "Parses back sequences within lines which are understood to be formatted"
  (let* ((parsed-lines (mapcar 'vc-rational-synergy--parse-formatted-line
			       lines))
	 (non-nil (remove-if-not (lambda (x) (not (eq nil x))) parsed-lines)))
    ;; In the rare case that the eventual list non-nil is empty,
    ;; return nil
    (if (eq 0 (length non-nil)) nil non-nil)))



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
	 
  
;;;; User Interface.


(defun vc-rational-synergy-message (format &rest format-string)
  "Either uses a message-box or message to display information

Based on `vc-rational-synergy-use-message-boxes' either use
messages or message-boxes"

  (when vc-rational-synergy-use-message-boxes
    (apply 'message-box format format-string))
  (message (apply 'format format format-string)))

(provide 'vc-rational-synergy-utilities)

;; vc-rational-synergy-utilities.el ends here
