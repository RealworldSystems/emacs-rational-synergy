;;; vc-rational-synergy-checkout.el --- IBM Rational Synergy checkout commands
 
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

(require 'vc-rational-synergy-command-to-string)
(require 'vc-rational-synergy-buffer)

(defun vc-rational-synergy--command-co (file-name)
  "Check out a particular file or directory
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (condition-case err
      (vc-rational-synergy-command-to-string 
       `("co" ,file-name))
    (error nil)))

(defun vc-rational-synergy--command-undo-co (file-name)
  "Undo the check out of a particular file or directory
This is a wrapper around `vc-rational-synergy-command-to-string'"
  (condition-case err
      (vc-rational-synergy-command-to-string 
       `("unuse" "-replace" "-delete" ,file-name))
    ;; Awkward enough, this actually raises an error even
    ;; on success, this is caused by unuse generating a
    ;; non-zero exit status.
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


;;;###autoload
(defun vc-rational-synergy-co-directory ()
  "Check out a directory from CM Synergy."
  (interactive)

  (with-vc-rational-synergy

   (catch 'exit
     ;; First check if this buffer is at all part of synergy, and
     ;; is in integration mode

     (condition-case err
	 (unless (vc-rational-synergy-buffer-directory-integrate)
	   (vc-rational-synergy-message
	    "The directory pointed to by this buffer is not in integrate state")
	   (throw 'exit nil)
       (error err
	      (vc-rational-synergy-message (error-message-string err))
	      (throw 'exit nil))))

     ;; First check if this buffer is modified, if such, bail out
     (when (buffer-modified-p) 
       (vc-rational-synergy-message
	"This buffer is modified, please revert or save first.")
       (throw 'exit nil))

     
     ;; Check if there are any buffers associated to the current work area,
     ;; and see if they are modified. Bail out if this is the case
     (when (vc-rational-synergy--buffers-modified-p)
       (vc-rational-synergy-message
	"There are buffers in modified state, please revert or save first.")
       (throw 'exit nil))
     
     ;; Now all things are go
     (let ((directory-name (vc-rational-synergy--buffer-directory)))
     (if (vc-rational-synergy--command-co directory-name)
	 (progn
	   (vc-rational-synergy--revert-buffers)
	   (vc-rational-synergy-message "Checkout complete: %s" directory-name))
       (vc-rational-synergy-message "Failed checking out: %s" directory-name))))))



;;;###autoload
(defun vc-rational-synergy-undo-co-file ()
  "Undo file checkout from CM Synergy."
  (interactive)
  (with-vc-rational-synergy
   (catch 'exit
     ;; First check if this buffer is at all part of synergy, and
     ;; is in integration mode

     (condition-case err
	 (unless (vc-rational-synergy-buffer-working)
	   (vc-rational-synergy-message
	    "The file pointed to by this buffer is not in working state")
	   (throw 'exit nil)
       (error err
	      (vc-rational-synergy-message (error-message-string err))
	      (throw 'exit nil))))
       
     
     ;; If buffer is modified, halt
     (when (buffer-modified-p) 
       (vc-rational-synergy-message 
	"This buffer is modified, please revert or save first")
       (throw 'exit nil))

     (unless (y-or-n-p "Really UNDO the checkout, all modifications are lost!")
       (throw 'exit nil))
     
     ;; Buffer is not modified, continue
     (vc-rational-synergy--command-undo-co (buffer-file-name))
     (revert-buffer nil t))
     (vc-rational-synergy-message "Undone: %s" (buffer-name))))


(defun vc-rational-synergy--attempt-undo-check-out-directory-files ()
  "Attempts full undo check out for all files in the current directory
If any checkin has failed, return those failures as list"
  (delq nil
	(mapcar 'vc-rational-synergy--command-undo-co
		(vc-rational-synergy--working-directory-files
		 (vc-rational-synergy--buffer-directory)))))


;;;###autoload
(defun vc-rational-synergy-undo-co-directory ()
  "Check out a directory from CM Synergy."
  (interactive)

  (with-vc-rational-synergy

   (catch 'exit
     ;; First check if this buffer is at all part of synergy, and
     ;; is in integration mode

     (condition-case err
	 (unless (vc-rational-synergy-buffer-directory-working)
	   (vc-rational-synergy-message
	    "The directory pointed to by this buffer is not in working state")
	   (throw 'exit nil)
       (error err
	      (vc-rational-synergy-message (error-message-string err))
	      (throw 'exit nil))))

     ;; First check if this buffer is modified, if such, bail out
     (when (buffer-modified-p) 
       (vc-rational-synergy-message
	"This buffer is modified, please revert or save first.")
       (throw 'exit nil))

     
     ;; Check if there are any buffers associated to the current work area,
     ;; and see if they are modified. Bail out if this is the case
     (when (vc-rational-synergy--buffers-modified-p)
       (vc-rational-synergy-message
	"There are buffers in modified state, please revert or save first.")
       (throw 'exit nil))

     ;; Be really sure
     (unless (y-or-n-p "Really UNDO the checkout, all modifications are lost!")
       (throw 'exit nil))

     ;; First check in all the files which are still in working
     ;; mode
     (let ((failed-files (vc-rational-synergy--attempt-undo-check-out-directory-files)))
       (when failed-files
	 (vc-rational-synergy-message
	  (concat "Stop undoing, failed to undo: "
		  (mapconcat (lambda (x) x) failed-files "; ")))
	 (throw 'exit nil)))

     
     ;; Now all things are go
     (let ((directory-name (vc-rational-synergy--buffer-directory)))
       (vc-rational-synergy--command-co directory-name)
       (vc-rational-synergy--revert-buffers)
       (vc-rational-synergy-message "Undone: %s" directory-name)))))

(provide 'vc-rational-synergy-checkout)
;;; vc-rational-synergy-checkout.el ends here
