;; Introduction:
;;   This module is used for setup and configure Git usage in Emacs
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_git.el")

(require 'tntLib)

(setq myPkgs
  '(magit
    git-timemachine
    diff-hl
    ))

(tntLib_install-myPkgs)
