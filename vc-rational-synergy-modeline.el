;;; vc-rational-synergy-modeline.el --- IBM Rational Synergy integration for Emacs
 
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

;;; Code:

(require 'vc-rational-synergy-command-to-string)
(require 'vc-rational-synergy-authentication)

(if (not (fboundp `redraw-modeline))
    (defalias `redraw-modeline `force-mode-line-update))

(defvar vc-rational-synergy-modeline-string nil
"CMSyn information, displayed in the modeline.")
(make-variable-buffer-local 'vc-rational-synergy-modeline-string)


(defun vc-rational-synergy-int-update-modeline (p-vc-rational-synergy-string)
  "Do put P-VC-RATIONAL-SYNERGY-STRING in the modeline.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-VC-RATIONAL-SYNERGY-STRING: the id for this file in the modeline
  Returns       : -"
  (setq vc-rational-synergy-modeline-string p-vc-rational-synergy-string)
  (set (intern "mode-line-buffer-identification") 'vc-rational-synergy-modeline-string)
  (when (get-buffer-window (current-buffer) 0)
    (redraw-modeline)
    )
  )

;;;###autoload
(defun vc-rational-synergy-update-modeline ()
  "Show file name, version and status in the modeline, retrieving the information asynchronously so user doesnt' have to wait.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Methodology   : Partly copied from ccm-update-modeline, made asynchronous
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-login)
  (when (buffer-file-name) ;; only if we are in a file there is version-information to display
    (let* ((l-filename (file-name-nondirectory (buffer-file-name)))
	   ;; ----------
	   ;; retrieve the output with proc with filter, mode-line is updated by filter
	   ;; ----------
	   l-proc l-string
	   )
      (setq comint-output-filter-functions
	    (push 'shell-strip-ctrl-m
		  comint-output-filter-functions))
      ;; ----------
      ;; set temp modeline-info (while waiting for the correct info from ccm)
      ;; ----------
      (setq l-string (format "%s-%s" l-filename "CMSynergyRetrievingVersion..."))
      (vc-rational-synergy-int-update-modeline l-string)
      ;; ----------
      ;; Changed: GR: calling `start-process' seems to give hanging
      ;; processes now and then. Changed to `call-process' therefore
      ;; ----------
      (setq l-string (vc-rational-synergy-command-to-string (format "ls -f \"%%version@@%%status\" %s" (vc-rational-synergy-platformify-path (buffer-file-name)))))
      ;; ----------
      ;; put the info from CM Synergy in the mode-line
      ;; ----------
      (message "!! CCM VERSION-String: -%s-" l-string)
      (save-match-data
	(let* 
	    (
	     ;; ----------
	     ;; Split the answer at : if there is no error-message
	     ;; there may be text before the answer: chop off, the answer is a line
	     ;; of its own, the last
	     ;; ----------
	     (l-ccm-version (if (and (string-match "\\(.*\n\\)*\\([^@\n]+\\)@@\\([^@\n]+\\)\n" l-string)
				     (not (string-match vc-rational-synergy-warning-error-output-regexp l-string))
				     )
				(format "%s:%s"(match-string 2 l-string) (match-string 3 l-string))
			      "CMSynergyVersion??"))
	     (l-buffer (current-buffer))
	     (l-writable-string (if buffer-read-only "-" ":"))
	     (l-filename (vc-rational-synergy-platformify-path (file-name-nondirectory (buffer-file-name))))
	     )
	  (vc-rational-synergy-int-update-modeline (format "%s%s%s" l-filename l-writable-string l-ccm-version))
	  )
	)
      )
    )
  (vc-rational-synergy-check-logout)
  )


(provide 'vc-rational-synergy-modeline)

;; vc-rational-synergy-modeline.el ends here
