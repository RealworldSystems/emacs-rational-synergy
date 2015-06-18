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

(defun vc-rational-synergy--update-modeline-version (version)
  "Displays the actual version information in the modeline"
  
  ;; Split the answer at : if there is no error-message
  ;; there may be text before the answer: chop off, the answer is a line
  ;; of its own, the last
  (save-match-data
    (let* (
	   (expression "\\(.*\n\\)*\\([^@\n]+\\)@@\\([^@\n]+\\)\n")
	   (ccm-version (if (and 
			     (string-match expression version)
			     (vc-rational-synergy-result-okay-p version))
			    (format "%s:%s" (match-string 2 l-string)
				    (match-string 3 l-string))
			  "CMSynergyVersion??"))
	   (writable-string (if buffer-read-only "-" ":")))
      (vc-rational-synergy-int-update-modeline 
       (format "%s%s%s" filename writable-string ccm-version)))))

;;;###autoload
(defun vc-rational-synergy-update-modeline ()
  "Show file name, version and status in the modeline"
  (interactive)

  (with-vc-rational-synergy
   (when (buffer-file-name) ;; only if we are in a file there is version-information to display
     (let* ((filename (file-name-nondirectory (buffer-file-name))))

       (with-vc-rational-synergy-comint-strip-ctrl-m
	(let ((version (vc-rational-synergy-command-to-string 
			`("ls" "-f" "\"%%version@@%%status\" %s" 
			  ,(vc-rational-synergy-platformify-path (buffer-file-name))))))

	  (when version
	    vc-rational-synergy--update-modeline-version version)))))))

(provide 'vc-rational-synergy-modeline)

;; vc-rational-synergy-modeline.el ends here
