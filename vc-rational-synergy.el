;;; vc-rational-synergy.el --- Installation of cmsyn, CM Synergy mode
 
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

(require 'cm-synergy-script-mode)             ;; Mode for editing scripts
(require 'vc-rational-synergy-menu)           ;; The menu

(define-minor-mode vc-cmsyn-mode
  "Minor-mode for files in a CM Synergy work-area"
  nil " CMSyn" vc-rational-synergy-mode-map
  (when vc-cmsyn-mode
    (vc-cmsyn-check-status-buffer) ;; also check here, update modeline goes async
    (vc-rational-synergy-update-modeline) ;; also updates status! FIX ME: when into VC this has to be removed!
    )
  )

(provide 'vc-rational-synergy)

;; vc-rational-synergy.el ends here
