;;; vc-rational-synergy-menu.el --- Installation of cmsyn, CM Synergy mode
 
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
 
;; This installs the menu for the IBM Rational Synergy emacs interface
 
;;; Code:

(defun vc-rational-synergy-toggle-auto-check-status-file-open (&optional p-var p-val)
  "Toggles checking file-status at opening of files, called from menu-bar or by customizing `vc-rational-synergy-auto-check-status-file-open-p'.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-VAR P-VAL: supplied when `vc-rational-synergy-auto-check-status-file-open-p' is customized:
		    symbol vc-rational-synergy-auto-check-status-file-open-p and value of this variable.
  Returns       : "
  (interactive)
  
  (when p-var (custom-set-default p-var p-val))
  (cond

   ;; FIX ME: when into VC this has to be removed!
   (p-val                   (add-hook 'find-file-hooks 'vc-cmsyn-check-set-vc-cmsyn-mode))
   ((and p-var (not p-val)) (remove-hook 'find-file-hooks 'vc-cmsyn-check-set-vc-cmsyn-mode))
   ;; called without parameters, now call this *with* parameters and toggled value
   (t                       (vc-rational-synergy-toggle-auto-check-status-file-open 
			     'vc-rational-synergy-auto-check-status-file-open-p
			     (not vc-cmsyn-auto-check-status-file-open-p)))))


(require 'loadhist) ;; because of calling function `feature-file'
(require 'easymenu) ;; Definition of the menu
(require 'vc-rational-synergy-about)          ;; The about message
(require 'vc-rational-synergy-authentication) ;; explicit login/logout
(require 'vc-rational-synergy-base)           ;; base functionality

(require 'vc-rational-synergy-customization-base)

(defvar vc-cmsyn-mode
  nil
  "*variable indicating if minor-mode vc-cmsyn-mode is switched on.
  Date          : May/2003
  Author        : Realworld Systems (GR)."
  )

(defun vc-cmsyn-info ()
  "Shows info for IBM Rational Synergy"
  (interactive)
  (info (expand-file-name "vc-cmsyn.info" 
			  (file-name-directory 
			   (feature-file 'vc-cmsyn-install)))))


;;;; Menu definition.

(easy-menu-define menu-bar-vc-rational-synergy-menu nil "IBM Rational Synergy menu"
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
	"---" 
	["Customize..."				  (customize-group 'vc-rational-synergy)]
	["Info..."				  vc-cmsyn-info]
	["About IBM Rational Synergy Mode"        vc-rational-synergy-about])
  )


;;;; Activate the menu

(defun vc-rational-synergy-menu-path-changed (p-var p-val)
  "Set custom-option `vc-rational-synergy-menu-path' and move menu if applicable.
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
  (easy-menu-add-item global-map (car p-val) menu-bar-vc-rational-synergy-menu (cdr p-val))
  )

(when (boundp 'menu-bar-final-items) (set-variable 'menu-bar-final-items (cons 'CMSynergy (symbol-value 'menu-bar-final-items))))

(defcustom vc-rational-synergy-menu-path
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
  :set 'vc-rational-synergy-menu-path-changed
  :group 'vc-rational-synergy
  )

;;;; Administrative options.


(provide 'vc-rational-synergy-menu)

;;; vc-rational-synergy-menu.el ends here
