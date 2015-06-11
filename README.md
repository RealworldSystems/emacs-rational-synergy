# emacs-rational-synergy
IBM Rational Synergy interface for Emacs

Installation:
1. Add this directory to your load-path;
    (setq load-path (cons <directory-path for this file> load-path))
2. Load the install file:
   (require "vc-cmsyn-install")
3. Put the line `(vc-cmsyn-auto-check-status-file-open-p t)' in
    the custom-set-variables section of your initialisation file
     (probably .emacs).
4. The CMSynergy menu is found in the menu-bar under Tools by default
   CMSynergy (which can be customized however by changing the custom-var
   `vc-cmsyn-menu-path' with the emacs-customization system)

