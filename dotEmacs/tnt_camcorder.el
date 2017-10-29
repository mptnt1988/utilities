;; Introduction:
;;   This module is used for set up screencast recorder
;; Note:
;;   + 'recordmydesktop' and 'ffmpeg' must be installed in Linux env
;;   + Emacs run in window mode
;;   + Add path to this file to 'load-path
;;       (add-to-list 'load-path "/path/to/this/file")
;;     then
;;       (load "tnt_camcorder.el")

(require 'tntLib)

(setq myPkgs
      '(
        camcorder
        ))

(tntLib_install-myPkgs)
