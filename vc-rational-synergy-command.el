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
(require 'vc-rational-synergy-sentinels)

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


(defvar vc-rational-synergy-process-start-mark
  nil
  "*buffer-local var used for storing begin of process in the vc-cmsyn output-buffer.
  Date          : Aug/2003
  Author        : Realworld Systems (GR)."
  )
(make-variable-buffer-local 'vc-rational-synergy-process-start-mark)


(defun vc-rational-synergy-run-command
  (info command-line &optional sentinel filter check-task-set)
  "Call the CM Synergy executable with COMMAND-LINE, using INFO.
Runs the CCM command inside a buffer, with a given command line. Uses a
SENTINEL and FILTER for output processing and checks if a particular task is
set if CHECK-TASk-SET is non-nil."

  ;; check task set, if applicable. It only applicable if option
  ;; vc-rational-synergy-check-default-task-set-p is set
  (when (and vc-rational-synergy-check-default-task-set-p check-task-set)
    (vc-cmsyn-check-task-set t))
  

  ;; Initialize defaults
  (unless sentinel (setq sentinel 'vc-rational-synergy-sentinel))
  (unless filter (setq filter 'vc-cmsyn-process-filter))
  (unless (listp command-line)
    (display-warning :warning "Old use of command-line in vc-rational-synergy-run-command")
    (setq command-line (vc-rational-synergy--split-string-carefully command-line)))
  
  (vc-rational-synergy-buffer--wind-to-end)
  
  (vc-rational-synergy-report (format "\n%s\n" info))
    
  (with-vc-rational-synergy-comint-strip-ctrl-m
    
   (let ((process (apply 'start-process vc-rational-synergy-binary-name
			 (vc-rational-synergy-buffer)
			 (vc-rational-synergy-binary-name)
			 command-line)))
     (vc-cmsyn-map-proc-and-buffer process (current-buffer))
     (set-process-sentinel process sentinel)
     (set-process-filter process filter)
     process)))

(defun vc-rational-synergy--split-string-carefully (p-string &optional p-separators)
  "splits string P-STRING such that parts within \" \" inside the string will stay 1 unit.
  Author        : Realworld Systems (MO)
  Date          : Jan/2005
  Parameters    : P-STRING: string to split
  Returns       : list with string-parts"
  (let* 
      (
       (l-string-to-split p-string)
       (l-splits)
       )
    (while (not (zerop (length l-string-to-split)))
      (if (string-match "^\\([^\"]*\\)\\(\"[^\"]*\"\\)\\(.*\\)" l-string-to-split)
	  (progn
	    (setq l-splits (nconc l-splits
				  (save-match-data (split-string (match-string 1 l-string-to-split) p-separators))
				  (list (match-string 2 l-string-to-split))
				  ))
	    (setq l-string-to-split (match-string 3 l-string-to-split))
	    )
	(setq l-splits (nconc l-splits (save-match-data (split-string l-string-to-split p-separators))))
	(setq l-string-to-split nil)
	)
      )
    l-splits
    )
  )

(defun vc-cmsyn-process-filter (p-proc p-string)
  "Insert the output string in process-buffer, at the end.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROC: running proc
                  P-STRING: outputstring
  Returns       : -"
  (set-buffer (vc-rational-synergy-buffer))
  (goto-char (point-max))
  (insert p-string)
  )

(defvar vc-cmsyn-buffer-frame
  nil
  "*The frame with the cmsyn-output-buffer.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  )

(provide 'vc-rational-synergy-command)

;; vc-rational-synergy-command.el ends here
