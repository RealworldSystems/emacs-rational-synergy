;;; vc-rational-synergy-buffer.el --- IBM Rational Synergy buffer

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
;;     
;; Maintainer:
;;     Sjoerd van Leent <sjoerd.van.leent@realworld-systems.com>
;;
;; Homepage: http://github.com/RealworldSystems/emacs-rational-synergy
;;
;; Keywords: ibm rational synergy telelogic vc version-control


;;; Commentary:

;; These functions deal with the IBM Rational Synergy buffer

;; Code:

(require 'vc-rational-synergy-modeline)

(defun vc-rational-synergy-buffer ()
  "Return the output-buffer for the ccm process.
If the buffer does not exist yet, create it within the current frame"
  (interactive)
  
  (save-excursion
    (selected-frame)
    (get-buffer-create vc-rational-synergy-buffer-name)))

(defun vc-rational-synergy-buffer--wind-to-end ()
  "Winds the synergy buffer to the end
Sets cursor at point-max"

  (save-excursion
    (set-buffer (vc-rational-synergy-buffer))
    (goto-char (point-max))
    (setq vc-rational-synergy-process-start-mark (point-marker))))



(defun vc-cmsyn-buffer-insert (p-message)
  "Insert message P-MESSAGE.in the vc-cmsyn output buffer
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-MESSAGE: message to insert
  Returns       : "
  (with-current-buffer (vc-rational-synergy-buffer)
    (goto-char (point-max))
    (insert (format "\n%s\n" p-message))
    )
  )

(defun vc-rational-synergy-report (p-message)
  "Report the message in minibuffer and in output-buffer.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-MESSAGE: message to report
  Returns       : "
  (message p-message)
  (vc-cmsyn-buffer-insert p-message)
  )

(defvar vc-cmsyn-proc-buffer-mappings
  nil
  "*Alist with mappings of filter-proc with the buffer it should work on.
  Date          : Apr/2003
  Author        : Realworld Systems (GR)."
  )

(defun vc-cmsyn-map-proc-and-buffer (p-proc p-buffer)
  "Store parameters in a cons in alist `vc-cmsyn-proc-buffer-mappings' for later retrieval in sentinel or filter for proc.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Paramehters    : P-BUFFER: Buffer to be retrieved within sentinel or filter for proc
                  P-PROC: key for the alist
  Returns       : -"
  (push (cons p-proc p-buffer) vc-cmsyn-proc-buffer-mappings)
  )

(defun vc-cmsyn-get-buffer-mapped-to-proc (p-proc)
  "Get the buffer that was mapped in combination with proc P-PR0C in variable `vc-cmsyn-proc-buffer-mappings'.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROC: The key for the mapping
  Returns       : buffer or nil"
  (cdr (assoc p-proc vc-cmsyn-proc-buffer-mappings))
  )

(defun vc-cmsyn-clear-buffer-mapping-for-proc (p-proc)
  "Remove the entry for proc P-PROC.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : P-PROC: Key for the buffer-proc-mapping
  Returns       : -"
  (setq vc-cmsyn-proc-buffer-mappings (assq-delete-all p-proc vc-cmsyn-proc-buffer-mappings))
  )

(defun vc-cmsyn-show-output-buffer ()
  "Switch to the output-buffer and put cursor at the end, this may be to the separate frame or within the current frame.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (let* 
      (
       (l-proc-buffer (vc-rational-synergy-buffer))
       (l-win (get-buffer-window l-proc-buffer 0))
       (l-frame (when l-win (window-frame l-win)))
       )
    (if l-frame
	(select-frame-set-input-focus l-frame)
      (pop-to-buffer l-proc-buffer)
      )
    (goto-char (point-max))
    )
  )

(defun vc-cmsyn-check-status-buffers ()
  "Check/reset status of all buffers in cmsyn mode, refreshes mode-lines.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : "
  (save-excursion
    (dolist (i-buffer (buffer-list))
      (set-buffer i-buffer)
      (when vc-cmsyn-mode
	(vc-cmsyn-check-status-buffer)
	(vc-rational-synergy-update-modeline) ;; async process, checks status again afterwards
	)
      )
    )
  )

(defun vc-cmsyn-check-status-buffer ()
  "Checks writable/readonly compared with file-attribs.
  Author        : Realworld Systems (GR)
  Date          : Apr/2003
  Parameters    : 
  Returns       : -"
  (when (buffer-file-name)
    (cond
     ((and (file-writable-p (buffer-file-name)) buffer-read-only)
      (toggle-read-only))
     ((and (not (file-writable-p (buffer-file-name))) (not buffer-read-only))
      (toggle-read-only))
     ))
  )

(provide 'vc-rational-synergy-buffer)

;; vc-rational-synergy-buffer.el ends here
