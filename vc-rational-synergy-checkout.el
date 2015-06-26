;;; vc-rational-synergy-checkin.el --- IBM Rational Synergy checkout commands
 
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
;;     
;; Maintainer:
;;     Sjoerd van Leent <sjoerd.van.leent@realworld-systems.com>
;;
;; Homepage: http://github.com/RealworldSystems/emacs-rational-synergy
;;
;; Keywords: ibm rational synergy telelogic vc version-control


;;; Commentary:

;; Supports checking out files, directories and/or tasks

;;; Code:

(defun vc-rational-synergy--command-co (file-name)
  "Check out a particular file
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (condition-case err
      (vc-rational-synergy-command-to-string 
       `("co" ,file-name))
    (error nil)))


;;;###autoload
(defun vc-rational-synergy-co-file ()
  "Check out a file from CM Synergy."
  (interactive)
  (with-vc-rational-synergy

   (catch 'exit
     ;; First check if this buffer is at all part of synergy, and
     ;; is in integration mode

     (condition-case err
	 (unless (vc-rational-synergy-buffer-integrate)
	   (vc-rational-synergy-message
	    "The file pointed to by this buffer is not in integrate state")
	   (throw 'exit nil)
       (error err
	      (vc-rational-synergy-message (error-message-string err))
	      (throw 'exit nil))))
       
     
     ;; If buffer is modified, halt
     (when (buffer-modified-p) 
       (vc-rational-synergy-message 
	"This buffer is modified, please revert or save first")
       (throw 'exit nil))
     
     ;; Buffer is not modified, continue
     (if (vc-rational-synergy--command-co (buffer-file-name))
	 (progn
	   (revert-buffer nil t)
	   (vc-rational-synergy-message "Checkout complete: %s" (buffer-name)))
       (vc-rational-synergy-message "Failed checking out: %s" (buffer-name))))))



(provide 'vc-rational-synergy-checkout)
;;; vc-rational-synergy-checkout.el ends here
