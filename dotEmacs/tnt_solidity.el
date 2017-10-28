;; Introduction:
;;   This module is used for setup and configure Emacs Ethereum's Solidity
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_solidity.el")

(require 'tntLib)

(setq myPkgs
  '(solidity-mode
    ))

(tntLib_install-myPkgs)
