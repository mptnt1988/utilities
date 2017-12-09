;;; tnt_camcorder --- Desktop recording
;;;---------------------------------------------------------
;;; Commentary:
;;  This module is used for set up screencast recorder
;;  + 'recordmydesktop' and 'ffmpeg' must be installed in Linux env
;;  + Emacs run in window mode
;;  Usage:
;;    (require 'tnt_camcorder)

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

(setq tntLib-myPkgs
  '(
    camcorder
    ))

(tntLib_install-myPkgs)

(provide 'tnt_camcorder)
;;; tnt_camcorder.el ends here
