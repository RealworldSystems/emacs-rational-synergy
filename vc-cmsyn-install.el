;;; vc-cmsyn-install.el --- Installation of cmsyn, CM Synergy mode
 
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
 
;; This installs vc-cmsyn in emacs.
;; See Installation instructions in vc-cmsyn.el for details
 
;;; Code:

(require 'loadhist) ;; because of calling function `feature-file'

(require 'vc-rational-synergy-about) ;; The about message can be accessed by the menu

(defvar vc-cmsyn-mode
  nil
  "*variable indicating if minor-mode vc-cmsyn-mode is switched on.
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  )



(defun vc-cmsyn-info ()
  "Shows info for IBM Rational Synergy"
  (interactive)
  (require 'loadhist)
  (info (expand-file-name "vc-cmsyn.info" 
			  (file-name-directory 
			   (feature-file 'vc-cmsyn-install)))))

(require 'easymenu)
(easy-menu-define menu-bar-vc-cmsyn-menu nil "IBM Rational Synergy menu"
  (list "IBM Rational Synergy"
	["Login..." vc-rational-synergy-login]
	["Logout..." vc-rational-synergy-logout]
	"---"
	["Show Default Task"			  vc-cmsyn-show-default-task   	    :keys "C-c RET d"]
	["Show Task Files"			  vc-cmsyn-show-task-files]
	["Open Task Files"			  vc-cmsyn-open-task-files]
	["Select Task"				  vc-cmsyn-select-task	   	    :keys "C-c RET s"]
;; 	["Create New Task" vc-cmsyn-create-task]
	"---"
	["Check Out This File"			  vc-cmsyn-co-file			  :active vc-cmsyn-mode  :keys "C-c RET o"]
	["Undo Check Out File"			  vc-cmsyn-undo-co-file			  :active vc-cmsyn-mode  :keys "C-c RET u"]
	["Checkout This Directory"		  vc-cmsyn-co-directory			  :active (or (equal major-mode 'dired-mode) vc-cmsyn-mode )]
	["Undo Checkout Directory"		  vc-cmsyn-undo-co-directory		  :active (or (equal major-mode 'dired-mode) vc-cmsyn-mode )]
	"---"
	["Check In This File"			  vc-cmsyn-ci-file			  :active vc-cmsyn-mode  :keys "C-c RET i"]
;;
;; TODO: Should really be enabled
;; 	["Check In This Directory" vc-cmsyn-ci-directory	      :active (or (equal major-mode 'dired-mode) vc-cmsyn-mode )]
;;
	["Register"			          vc-cmsyn-register-file  	          :active vc-cmsyn-mode  :keys "C-c RET r"]
	["Register Directory and Files"	          vc-cmsyn-create-directory		  :active (or (equal major-mode 'dired-mode) vc-cmsyn-mode )]
	"---"
	["Check In Task"			  vc-cmsyn-ci-task	                  :keys "C-c RET t"]
	"---"
	["Update Modeline"			  vc-cmsyn-update-modeline]
	"---"
	["Show Properties"	   	          vc-cmsyn-properties			  :active vc-cmsyn-mode  :keys "C-c RET p"]
	'("Show history"
	  ["This file (graphics)"		  vc-cmsyn-history-file-graphics	  :active vc-cmsyn-mode  :keys "C-c RET h"]
	  ["This file (details)"		  vc-cmsyn-history-file-details		  :active vc-cmsyn-mode ]
	  ["This directory (graphics)"		  vc-cmsyn-history-directory-graphics	  :active (or (equal major-mode 'dired-mode) vc-cmsyn-mode )]
	  ["This directory (details)"		  vc-cmsyn-history-directory-details	  :active (or (equal major-mode 'dired-mode) vc-cmsyn-mode )]
	  )
	'("Compare"
	  ["With Previous Version"	          vc-cmsyn-ediff			  :active vc-cmsyn-mode ]
	  ["With Other Version"		          vc-cmsyn-ediff-other-version		  :active vc-cmsyn-mode ]
	  ["Versions"			          vc-cmsyn-ediff-versions		  :active vc-cmsyn-mode ]
	  )
	["Set CCM_ADDR..."			  vc-cmsyn-set-ccmAddr			  :visible vc-cmsyn-enable-set-ccm-addr-p]
	"---" 
	["Customize..."				  (customize-group 'vc-cmsyn-user)]
	["Customize Admin..."			  (customize-group 'vc-cmsyn-admin)	  :visible vc-cmsyn-customize-admin-button-p]
	["Info..."				  vc-cmsyn-info]
	["About IBM Rational Synergy Mode"        vc-rational-synergy-about])
  )

(defcustom vc-cmsyn-buffer-frame-width 90
  "The default width of the CM Synergy output-buffer-frame."
  :tag "CM Synergy output buffer frame width"
  :type 'integer
  :group 'vc-cmsyn-user)

(defcustom vc-cmsyn-buffer-frame-height 50
  "The default height of the CM Synergy output-buffer-frame."
  :tag "CM Synergy output buffer frame height"
  :type 'integer
  :group 'vc-cmsyn-user)

(defcustom vc-cmsyn-command-end-jump-to-output-buffer-p t
  "Jump to CM Synergy output buffer at end of CM Synergy commands?"
  :tag "Jump to Output-buffer After Commands?"
  :type 'boolean
  :group 'vc-cmsyn-user)

(defcustom vc-cmsyn-register-checks-in-p t
  "If this is non-nil registering a file into CM Synergy will check it in at the same time."
  :tag "Check a file in when registering?"
  :type 'boolean
  :group 'vc-cmsyn-user)

(defcustom vc-cmsyn-iconify-frame-when-ccm-gui
  t
  "*Boolean indicating if the current emacs-frame should be iconified when a ccm gui is called.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Iconify emacs when selecting tasks?"
  :type 'boolean
  :group 'vc-cmsyn-user
  )

(defun vc-cmsyn-menu-path-changed (p-var p-val)
  "Set custom-option `vc-cmsyn-menu-path' and move menu if applicable.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : "
  ;; ----------
  ;; remove an evt. old menu
  ;; ----------
  (when (boundp p-var)
    (easy-menu-remove-item global-map (car (symbol-value p-var)) "CMSynergy")
    (when (not (string-equal "menu-bar" (caar p-val))) ;; make sure menu-bar is the 1st entry
      (setq p-val (cons (cons "menu-bar" (car p-val)) (cdr p-val)))
      )
    )
  (custom-set-default p-var p-val)
  (easy-menu-add-item global-map (car p-val) menu-bar-vc-cmsyn-menu (cdr p-val))
  )

(when (boundp 'menu-bar-final-items) (set-variable 'menu-bar-final-items (cons 'CMSynergy (symbol-value 'menu-bar-final-items))))

(defcustom vc-cmsyn-menu-path
 (cons (list "menu-bar") "separator-vc") 
  "*Indicates the place of the CM Synergy menu in the menu-bar (no elements -so nil- is at top).
path-elements have to be checked in the menu-bar, examples: files for File-menu, edit, search, buffer, options.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Menu Path for CM Synergy menu"
  :type '(cons :tag "Menu-path"
	       (repeat (string     :tag "Path element (like \`tools' or \`menu-bar')"))
	       (string	      :tag "Optional before item like separator-vc or help")
	       )
  :set 'vc-cmsyn-menu-path-changed
  :group 'vc-cmsyn-user
  )

;; ----------
;; ADMIN options
;; ----------
(defgroup vc-cmsyn-admin nil
  "Admin-Customization options for CM Synergy mode.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "CM Synergy mode Admin options"
  :group 'programming
  :group 'tools
  )

(defcustom vc-cmsyn-register-directory-and-files-filter
  (list "~$" ",v$" "#$")
  "user, pwd and synergy-database to use for auto-login"
  :tag "CM Synergy auto-login-data"
  :type '(repeat
	  (regexp :tag "Regexp for files that should not be registered with `Register directory + files'")
	  )
  :group 'vc-cmsyn-admin)

(defcustom vc-cmsyn-auto-check-in-directory-p
  nil
  "*Should vc-cmsyn check-in the directory immediately again when a file is checked out in this dir?
With `Collaborative' personal work-areas the default Synergy-behaviour is to check out a directory when a file is checked out.
It may be better to keep the directories in integrate state so it's the same for everyone..
  Date          : Nov/2003
  Author        : Realworld Systems (GR)."
  :tag "Auto check-in directory on file-checkout?"
  :type 'boolean
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-customize-admin-button-p ;; take the default from the roos-Custom-save-option-central option, *if* present at all (else nil)
  (when (boundp 'roos-Custom-save-option-central-button-p) (symbol-value 'roos-Custom-save-option-central-button-p))
  "*determines if button for customizing admin-options should be present in CM Synergy-menu, default not, only allowed for specific users.
  Date          : Jul/2003
  Author        : Realworld Systems (GR)."
  :tag "Customize-admin-options button?"
  :type 'boolean
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-check-default-task-set-p
  nil
  "*Boolean indicating if it has to be checked if a default task is set before performing a ccm update.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Check for default task set?"
  :type 'boolean
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-query-create-file-type
  t
  "*Boolean indicating if type of file has to be queried before registering a file in ccm.
Typically this will be organized within CM Synergy already so it goes automatically.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Query File-type?"
  :type 'boolean
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-query-create-file-version
  t
  "*Boolean indicating if version of file has to be queried before registering a file in ccm.
Typically this will be organized within CM Synergy already so it goes automatically.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Query File-version?"
  :type 'boolean
  :group 'vc-cmsyn-admin
  )

(defun vc-cmsyn-toggle-auto-check-status-file-open (&optional p-var p-val)
  "Toggles checking file-status at opening of files, called from menu-bar or by customizing `vc-cmsyn-auto-check-status-file-open-p'.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-VAR P-VAL: supplied when `vc-cmsyn-auto-check-status-file-open-p' is customized:
		    symbol vc-cmsyn-auto-check-status-file-open-p and value of this variable.
  Returns       : "
  (interactive)
;;   (message "!! vc-cmsyn-toggle-auto-check-status-file-open: -%s- and -%s-" p-var p-val)
  (when p-var (custom-set-default p-var p-val))
  (cond
   (p-val
    (add-hook 'find-file-hooks 'vc-cmsyn-check-set-vc-cmsyn-mode) ;; FIX ME: when into VC this has to be removed!
    )
   ((and p-var (not p-val))
    (remove-hook 'find-file-hooks 'vc-cmsyn-check-set-vc-cmsyn-mode)
    )
   (t
    ;; ----------
    ;; called without parameters, now call this *with* parameters and
    ;; toggled value
    ;; ----------
    (vc-cmsyn-toggle-auto-check-status-file-open 'vc-cmsyn-auto-check-status-file-open-p (not vc-cmsyn-auto-check-status-file-open-p))
    )
   )
  )

(defcustom vc-cmsyn-auto-check-status-file-open-p
  1
  "*Indicates if cmsynergy status should be checked for on opening of files.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "Auto check status on file open?"
  :type 'boolean
  :set 'vc-cmsyn-toggle-auto-check-status-file-open
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-buffer-name "*vc-cmsyn*"
  "Name of buffer in which to log CM Synergy actions."
  :tag "CM Synergy output buffer name"
  :type 'string
  :group 'vc-cmsyn-admin)


(defcustom vc-cmsyn-developers-gui-exe-name "cmsynergy"
  "The CM Synergy Developers GUI executable name"
  :tag "CM Synergy Developers GUI executable name"
  :type 'string
  :group 'vc-cmsyn-admin)

(defcustom vc-cmsyn-version-object-separator
  "~"
  "The CM Synergy object-version separator, this is configured in CM Synergy, separates object-name and version in 4-part-names."
  :tag "Project Version Object Separator"
  :type '(character :tag "CM Synvergy Separator-char")
  :group 'vc-cmsyn-admin)

(defcustom vc-cmsyn-no-differences-found-string
  "(No differences found)"
  "The string CM Synergy outputs for indicating that there are no differences when comparing releases."
  :tag "No differences found string"
  :type 'string
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-parallel-versions-string
  "\\(Warning: \\)?Parallel \\(versions\\|branches\\) exist for "
  "*String in CM Synergy output when a parallel version exists.
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  :tag "String output by CM Synergy when a parallel version exists"
  :type 'string
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-already-checked-out-regexp
  "Warning: Checking out from a writable object is prohibited."
  "*Regexp for checking CM Synergy output to see if a file was checked out correctly.
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  :tag "String output by CM Synergy when a writable file is tried to checkout"
  :type 'string
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-checked-out-status-list
  '("working" "visible")
  "*Regexp for checking CM Synergy output to see if a file was checked out correctly.
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  :tag "String output by CM Synergy when a writable file is tried to checkout"
  :type 'list
  :group 'vc-cmsyn-admin
  )

(defcustom vc-cmsyn-enable-set-ccm-addr-p
  t
  "*Custom-option indicating if button for setting CCM_ADDR should should be enabled.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  :tag "CCM_ADDR button?"
  :type 'boolean
  :group 'vc-cmsyn-admin
  )

(provide 'vc-cmsyn-install)

;;; vc-cmsyn-install.el ends here
