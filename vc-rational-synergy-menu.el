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


(let ((dir-mode-check '(or (equal major-mode 'dired-mode) vc-cmsyn-mode )))
  (easy-menu-define menu-bar-vc-rational-synergy-menu nil "IBM Rational Synergy menu"

    `("IBM Rational Synergy"
   
      ["Login..."           vc-rational-synergy-login             :keys "C-c RET l"]
      ["Logout..."          vc-rational-synergy-logout]
      ["Current user"       vc-rational-synergy-logged-on-user]
      ["Status"             vc-rational-synergy-status]
      "---"
      
      ["Show Default Task"  vc-rational-synergy-default-task      :keys "C-c RET d"]
      ["Show Task Files"    vc-rational-synergy-show-task-files]
      ["Open Task Files"    vc-rational-synergy-open-task-files]
      ["Select Task"	    vc-rational-synergy-select-task       :keys "C-c RET s"]
      ["Check In Task"	    vc-rational-synergy-ci-task           :keys "C-c RET t"]

      ;; ["Create New Task" vc-rational-synergy-create-task]
      "---"
      
      ("Buffer"
       ["Show project"      vc-rational-synergy-current-project :active vc-cmsyn-mode]
       ["History"           vc-rational-synergy-history         :active vc-cmsyn-mode
                                                                :keys "C-c RET h"])

      ("File"
	
       ["Checkout"      vc-rational-synergy-co-file        :active vc-cmsyn-mode
                                                           :keys "C-c RET o"]
       ["Undo Checkout" vc-rational-synergy-undo-co-file   :active vc-cmsyn-mode
                                                           :keys "C-c RET u"]
       ["Check In"      vc-rational-synergy-ci-file        :active vc-cmsyn-mode
                                                           :keys "C-c RET i"]
       ["Register"      vc-rational-synergy-register-file  :active vc-cmsyn-mode
	                                                   :keys "C-c RET r"])
      ("Directories"
       ["Checkout"	 vc-rational-synergy-co-directory       :active ,dir-mode-check]
       ["Undo Checkout" vc-rational-synergy-undo-co-directory   :active ,dir-mode-check]
       ["Check In"      vc-rational-synergy-ci-directory        :active ,dir-mode-check]
       ["Register"      vc-rational-synergy-register-directory  :active ,dir-mode-check]
	)
      
      ["Update Modeline" vc-rational-synergy-update-modeline]
      "---"

      ["Show Properties" vc-rational-synergy-properties     :active vc-cmsyn-mode
                                                      :keys "C-c RET p"]

      ["Customize..."		 (customize-group 'vc-rational-synergy)]
      ["About..."               vc-rational-synergy-about :keys "C-c RET a"])))

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

;;;; Define actual key bindings
;; Key Bindings
(defvar vc-rational-synergy-mode-map
  (make-sparse-keymap)
  "Map for the special keys in minor-mode vc-cmsyn-mode."
  )

(defun vc-rational-synergy--dk (&rest args)
  "Defines keys into the IBM Rational Synergy mode map"
  (apply 'define-key vc-rational-synergy-mode-map args))

(vc-rational-synergy--dk (kbd "C-c C-m o") 'vc-rational-synergy-co-file)
(vc-rational-synergy--dk (kbd "C-c C-m u") 'vc-rational-synergy-undo-co-file)
(vc-rational-synergy--dk (kbd "C-c C-m i") 'vc-rational-synergy-ci-file)
(vc-rational-synergy--dk (kbd "C-c C-m h") 'vc-rational-synergy-history)
(vc-rational-synergy--dk (kbd "C-c C-m r") 'vc-rational-synergy-register-file)
(define-key global-map (kbd "C-c C-m d") 'vc-rational-synergy-default-task)
(define-key global-map (kbd "C-c C-m t") 'vc-rational-synergy-ci-task)
(define-key global-map (kbd "C-c C-m s") 'vc-rational-synergy-select-task)
(define-key global-map (kbd "C-c C-m p") 'vc-rational-synergy-properties)
(define-key global-map (kbd "C-c C-m a") 'vc-rational-synergy-about)
(define-key global-map (kbd "C-c C-m l") 'vc-rational-synergy-login) ;; login


(provide 'vc-rational-synergy-menu)

;;; vc-rational-synergy-menu.el ends here
