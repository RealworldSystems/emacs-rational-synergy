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
(require 'vc-rational-synergy-modeline)       ;; modeline functionality

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


(let ((dir-mode-check '(or (equal major-mode 'dired-mode) vc-cmsyn-mode ))
      (p (lambda (i) (make-symbol (concat "vc-rational-synergy-" (symbol-name i))))))
  (easy-menu-define menu-bar-vc-rational-synergy-menu nil "IBM Rational Synergy menu"

    `("IBM Rational Synergy"
   
      ["Login..."           ,(apply p '(login))]
      ["Logout..."          ,(apply p '(logout))]
      "---"
      
      ["Show Default Task"  ,(apply p '(show-default-task)) :keys "C-c RET d"]
      ["Show Task Files"    ,(apply p '(show-task-files))]
      ["Open Task Files"    ,(apply p '(open-task-files))]
      ["Select Task"	    ,(apply p '(select-task))       :keys "C-c RET s"]
      ["Check In Task"	    ,(apply p '(ci-task))           :keys "C-c RET t"]

      ;; ["Create New Task" ,(apply p '(create-task]
      "---"
      
      ("File"
	
       ["Checkout"      ,(apply p '(co-file))        :active ,vc-cmsyn-mode
                                                     :keys "C-c RET o"]
       ["Undo Checkout" ,(apply p '(undo-co-file))   :active vc-cmsyn-mode
                                                     :keys "C-c RET u"]
       ["Checkin"       ,(apply p '(ci-file))        :active vc-cmsyn-mode
                                                     :keys "C-c RET i"]
       ["Register"      ,(apply p '(register-file))  :active vc-cmsyn-mode
	                                             :keys "C-c RET r"])
      ("Directories"
       ["Checkout"	 ,(apply p '(co-directory))     :active ,dir-mode-check]
       ["Undo Checkout" ,(apply p '(undo-co-directory)) :active ,dir-mode-check]
       ["Register"      ,(apply p '(create-directory))  :active ,dir-mode-check]
	;; TODO: Should really be enabled
	;;["Check In"    ,(apply p '(ci-directory))   :active ,dir-mode-check]
	)
      "---"
      
      
      ["Update Modeline" ,(apply p '(update-modeline))]
      "---"

      ["Show Properties" ,(apply p '(properties))     :active vc-cmsyn-mode
                                                      :keys "C-c RET p"]

      ("History"
       ["File (graphics)" ,(apply p '(history-file-graphics)) :active vc-cmsyn-mode
	                                                      :keys "C-c RET h"]
       ["File (details)"       ,(apply p '(history-file-details))  :active vc-cmsyn-mode ]
       ["Directory (graphics)" ,(apply p '(history-directory-graphics)) :active ,dir-mode-check]
       ["Directory (details)"  ,(apply p '(history-directory-details))  :active ,dir-mode-check])

      ("Compare"
       ["With Previous Version" ,(apply p '(ediff))               :active vc-cmsyn-mode ]
       ["With Other Version"    ,(apply p '(ediff-other-version)) :active vc-cmsyn-mode ]
       ["Versions"              ,(apply p '(ediff-versions))      :active vc-cmsyn-mode ])
      "---" 

      ["Customize..."		 (customize-group 'vc-rational-synergy)]
      ["Info..."		,(apply p '(info))]
      ["About..."               ,(apply p '(about))])))



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
