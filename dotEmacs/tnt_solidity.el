;; Introduction:
;;   This module is used for setup and configure Emacs Ethereum's Solidity
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_solidity.el")

(require 'tntLib)

(setq myPkgs '(solidity-mode
               ))

(tntLib_install-myPkgs)

(require 'solidity-mode)

;; mptnt1988:
;; Currently solidity-mode hardcodes :command property to "/usr/bin/solc"
;; This is to reset it:
(setf (flycheck-checker-get (intern "solidity-checker") 'command)
      '("solcjs" "--bin" source-inplace))

(defun tnt_solidity-set-checker-executable ()
  (setq flycheck-solidity-checker-executable "solcjs"))
;; mptnt1988:
;; Problem with solidity-mode hook, remove it before adding new hook
(setq solidity-mode-hook '())
(add-hook 'solidity-mode-hook 'tnt_solidity-set-checker-executable)
