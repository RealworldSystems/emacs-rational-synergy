;;; vc-rational-synergy-base.el --- IBM Rational Synergy integration for Emacs
 
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

(require 'vc-rational-synergy-command-to-string)
(require 'vc-rational-synergy-command)
(require 'vc-rational-synergy-utilities)
(require 'vc-rational-synergy-authentication)
(require 'vc-rational-synergy-modeline)
(require 'vc-rational-synergy-project)
(require 'vc-rational-synergy-task)
(require 'vc-rational-synergy-checkin)
(require 'vc-rational-synergy-checkout)
(require 'vc-rational-synergy-register)
(require 'vc-rational-synergy-history)

(require 'vc-rational-synergy-administration-customization)
(require 'vc-rational-synergy-user-customization)

;;;###autoload
(defun vc-cmsyn-history-file-graphics ()
  "Display file history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (buffer-file-name))
	 (l-command (format "history %s -g" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-rational-synergy-run-command "Retrieving history..." l-command 'vc-cmsyn-sentinel-history-graphics)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-history-file-details ()
  "Display file history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (buffer-file-name))
	 (l-command (format "history %s" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-rational-synergy-run-command "Retrieving history..." l-command)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-history-directory-graphics ()
  "Display directory history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
	 (l-command (format "history %s -g" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-rational-synergy-run-command "Retrieving history..." l-command 'vc-cmsyn-sentinel-history-graphics)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-history-directory-details ()
  "Display directory history.
  Date          : Apr/2003
  Parameters    : 
  Returns       : 
  Author        : Realworld Systems (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (let* (
	 (l-filename (if (equal major-mode 'dired-mode) (if (listp dired-directory) (car dired-directory) dired-directory) (file-name-directory (buffer-file-name))))
	 (l-command (format "history %s" (vc-rational-synergy-platformify-path l-filename)))
	 )
    (vc-rational-synergy-run-command "Retrieving history..." l-command)
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun vc-cmsyn-check-set-vc-cmsyn-mode ()
  "Checks if file is in a CM Synergy work-area and sets vc-cmsyn-mode if applicable.
NB: called from within a hook, careful with errors!
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (condition-case l-outer-error
      (progn
	(message (buffer-file-name))
	(let ( (responsible (vc-cmsyn-responsible-p (buffer-file-name)))
	       (mode-on (not vc-cmsyn-mode)) )
	  (when (and responsible mode-on)
	    (condition-case l-error
		(vc-cmsyn-mode) ;; switch on
	      (error (format "Switching %s to CM Synergy mode failed: %s" 
			     (buffer-file-name) 
			     (error-message-string l-error)))))))
    (error (message "Checking for CM Synergy Workarea failed: %s" 
		    (error-message-string 
		     l-outer-error)))))

(define-minor-mode vc-cmsyn-mode
  "Minor-mode for files in a CM Synergy work-area"
  nil " CMSyn" vc-rational-synergy-mode-map
  (when vc-cmsyn-mode
    (vc-cmsyn-check-status-buffer) ;; also check here, update modeline goes async
    (vc-rational-synergy-update-modeline) ;; also updates status! FIX ME: when into VC this has to be removed!
    )
  )

;; ----------
;; EDIFF for CMSynergy
;; ----------
;;;###autoload
(defun vc-cmsyn-ediff-versions ()
  "   This asks for versions to compare, then shows both versions of current file, using ediff.
                  (unlike vc-version-diff)
  Date          : Mar/2001
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld OO Systems B.V. (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-v1            (read-string "Older version: "))
	 (l-v2            (read-string "Newer version: "))
	 (l-object-name   (vc-rational-synergy-command-to-string (format "ls -f \"%%objectname\" %s" (vc-rational-synergy-platformify-path l-file))))
	 l-low-buffer l-high-buffer l-string1 l-string2 l-file1 l-file2
	 )
      (if (not (string-match vc-rational-synergy-object-name-regexp l-object-name))
	  (error "Couldnt retrieve the right file-information from CMSynergy: %s" l-object-name)
	(save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
	(setq l-file1 (replace-match l-v1 nil nil l-object-name 1))
	(setq l-file2 (replace-match l-v2 nil nil l-object-name 1))
	(setq l-string1 (vc-rational-synergy-command-to-string (format "cat %s" l-file1)))
	(when (string-match vc-rational-synergy-warning-error-output-regexp l-string1)
	  (error (format "CM Synergy Error: %s" l-string1)))
	(setq l-string2 (vc-rational-synergy-command-to-string (format "cat %s" l-file2)))
	(when (string-match vc-rational-synergy-warning-error-output-regexp l-string2)
	  (error (format "CM Synergy Error: %s" l-string2)))
	(save-excursion
	  (setq l-low-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-v1))))
	  (erase-buffer)
	  (insert l-string1)
	  (when (equal system-type 'windows-nt)
	    (goto-char (point-min))
	    (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	    (goto-char (point-max))
	    (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	    )
	  )
	(save-excursion
	  (setq l-high-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-v2))))
	  (insert l-string2)
	  (when (equal system-type 'windows-nt)
	    (goto-char (point-min))
	    (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	    (goto-char (point-max))
	    (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	    )
	  )
	(ediff-buffers l-low-buffer l-high-buffer)
	)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-ediff-other-version ()
  "   This asks for versions to compare, then shows both versions of current file, using ediff.
                  (unlike vc-version-diff)
  Date          : Mar/2001
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld OO Systems B.V. (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when  (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-v1            (read-string "Other version: "))
	 (l-object-name   (vc-rational-synergy-command-to-string (format "ls -f \"%%objectname\" %s" (vc-rational-synergy-platformify-path l-file))))
	 (l-high-buffer    (current-buffer))
	 l-low-buffer l-string1 l-string2 l-file1 l-file2
	 )
      (if (not (string-match vc-rational-synergy-object-name-regexp l-object-name))
	  (error "Couldnt retrieve the right file-information from CMSynergy: %s" l-object-name)
	(save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
	(setq l-file1 (replace-match l-v1 nil nil l-object-name 1))
	(setq l-string1 (vc-rational-synergy-command-to-string (format "cat %s" l-file1)))
	(when (string-match vc-rational-synergy-warning-error-output-regexp l-string1)
	  (error (format "CM Synergy Error: %s" l-string1)))
	(save-excursion
	  (setq l-low-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-v1))))
	  (erase-buffer)
	  (insert l-string1)
	  (when (equal system-type 'windows-nt)
	    (goto-char (point-min))
	    (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	    (goto-char (point-max))
	    (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	    )
	  )
	(ediff-buffers l-low-buffer l-high-buffer)
	)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )

;;;###autoload
(defun vc-cmsyn-ediff ()
  "   Compare the current version with the parent version, using ediff.
  Date          : Mar/2001
  Parameters    : 
  Returns       : 
  Methodology   : 
  Author        : Realworld OO Systems B.V. (GR)."
  (interactive)
  (vc-rational-synergy-check-session)
  (when  (not (vc-cmsyn-responsible-p (buffer-file-name))) ;; 'cheap' test: it may not have been registered still!
    (error "File %s is not in a CM Synergy Workarea!" (buffer-file-name))
    )
  (save-excursion
    (let* 
	(
	 (l-file          (buffer-file-name))
	 (l-object-name   (vc-rational-synergy-command-to-string (format "ls -f \"%%objectname\" %s" (vc-rational-synergy-platformify-path l-file))))
	 (l-high-buffer   (current-buffer))
	 l-parent-object-name l-low-buffer l-parent-object-name-string l-parent-object-name l-string1 l-lines
	 )
      (when (not (string-match vc-rational-synergy-object-name-regexp l-object-name))
	(error "Couldnt retrieve the right file-information from CMSynergy for object: %s" l-object-name)
	)
      (save-match-data (when (string-match "\\(.*\\)\n$" l-object-name) (setq l-object-name (match-string 1 l-object-name))))
      (setq l-parent-object-name (vc-cmsyn-parent-4-part-name l-object-name))
      (cond ((not l-parent-object-name)
	     (error "No ancestor found of %s" l-file))
	    ((listp l-parent-object-name)
	     (setq l-parent-object-name (completing-read "More than 1 ancestor, choose which 1 to compare [tab to complete]: " l-parent-object-name))
	     )
	    )
     (setq l-string1 (vc-rational-synergy-command-to-string (format "cat %s" l-parent-object-name)))
      (when (string-match vc-rational-synergy-warning-error-output-regexp l-string1)
	      (error (format "CM Synergy Error: %s" l-string1)))
      (save-excursion
	(setq l-low-buffer (set-buffer (get-buffer-create (format "*vc-cmsyn-ediff-%s*" l-parent-object-name))))
	(erase-buffer)
	(insert l-string1)
	(when (equal system-type 'windows-nt)
	  (goto-char (point-min))
	  (while (re-search-forward "[\r]+$" nil t) (replace-match ""))
	  (goto-char (point-max))
	  (when (re-search-backward "\C-z\\=" nil t) (replace-match ""))
	  )
	)
      (ediff-buffers l-low-buffer l-high-buffer)
      )
    )
  (vc-rational-synergy-check-session-pause)
  )

(defun vc-cmsyn-parent-4-part-name (p-4-part-name)
  "Retrieve the parent-4-part-name of file in current-buffer.
  Author        : Realworld Systems (GR)
  Date          : May/2003
  Parameters    : 
  Returns       : String or list with strings (if more than 1 ancessor)"
  (let* 
      (
      (l-parent-object-name-string (vc-rational-synergy-command-to-string (format "query \"is_predecessor_of ('%s')\" -f \"%%objectname\"" p-4-part-name)))
       (l-lines (split-string l-parent-object-name-string "\n"))
       )
    (if (equal (length l-lines) 1)
	(nth 1 (split-string (car l-lines)))
      (let ((l-list (mapcar (lambda (p-line)
			      (nth 1 p-line) ;; format is <nr>) <objectname>
			      )
			    l-lines)))
	l-list
	)
      )
    )
  )

(defun vc-cmsyn-responsible-p (p-file &optional p-prev p-absolute)
 "Check if the file P-FILE is located in a CM Synergy workarea"
 (let* ( (l-file     (if p-absolute
			 p-file
		       (vc-rational-synergy-unixify-path p-file))       )   ;; unixify
	 (l-path     (file-name-directory l-file)            )
	 (l-expanded (expand-file-name "_ccmwaid.inf" l-path))
	 (l-up       (expand-file-name ".." l-path)          )
	 (l-up-trail (if (string-match "/$" l-up)
			 l-up
		       (concat l-up "/"))                    )
	 (l-prev     (if p-prev p-prev "")                   ) )
   

   (cond ((file-exists-p l-expanded)
	  l-expanded)
	 ((string= l-prev l-expanded)
	  '())
	 (t
	  (vc-cmsyn-responsible-p l-up-trail l-expanded t)))))


;;; HERE THINGS ARE REFACTORED

;;;; Properties of a file

(defun vc-rational-synergy--beautify-property (a-symbol)
  "Given a symbol, uppercases and eradicates the hyphen"
  (let* ((name (symbol-name a-symbol))
	 (splitted (split-string name "\\-")))
    (mapconcat 'capitalize splitted " ")))

(defun vc-rational-synergy--command-properties (file-name)
  "Acquire the properties of a given object
This is a wrapper around `vc-rational-synergy-command-w/format-to-list'"
  (condition-case err
      (let* ((order '(owner status create-time modify-time 
			   platform release created-in local-to task))
	     (result (car (apply 'vc-rational-synergy-command-w/format-to-list
				 `("prop" ,file-name) order)))
	     ret v)
	(when result
	  (dotimes (i (length order) ret)
	    (setq ret (cons `(,(vc-rational-synergy--beautify-property
				(nth i order)) . ,(nth i result)) ret)))))
    (error nil)))
  

;;;###autoload
(defun vc-rational-synergy-properties ()
  "Display the properties of the file in the work buffer."
  (interactive)
  (with-vc-rational-synergy
   (when (vc-rational-synergy--check-buffer-assoc)
     (let ((a-list (vc-rational-synergy--command-properties (buffer-file-name)))
	   (buffer (vc-rational-synergy-buffer))
	   (file-name (buffer-file-name)))
       (when a-list
	 (pop-to-buffer buffer)
	 (erase-buffer)
	 (insert (vc-rational-synergy--tabular-props a-list 
						     file-name))))))
  (message ""))


(provide 'vc-rational-synergy-base)

;;; vc-rational-synergy-base.el ends here
