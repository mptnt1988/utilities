;;; tnt_solidity --- Setup and configure Emacs Ethereum's Solidity
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_solidity)

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

(setq tntLib-myPkgs
  '(
    solidity-mode
    ))

;; (defun tnt_solidity_remove-elc ()
;;   "Remove solidity-mode.elc."
;;   (message "Auto remove solidity-mode.elc by mptnt1988")
;;   (let*
;;       ((tntcd "cd ~/.emacs.d/elpa/solidity-mode-*")
;;        (tntrm "rm -f solidity-mode.elc"))
;;     (call-process-shell-command
;;      (concat tntcd " && "
;;              tntrm))))

(tntLib_install-myPkgs) ;; 'tnt_solidity_remove-elc)

(require 'solidity-mode)

;; mptnt1988:
;; Currently solidity-mode hardcodes :command property to "/usr/bin/solc"
;; This is to reset it:
(setf (flycheck-checker-get (intern "solidity-checker") 'command)
      '("solcjs" "--bin" source-inplace))

(defun tnt_solidity-set-checker-executable ()
  "Set overwriting variable flycheck checker."
  (setq flycheck-solidity-checker-executable "solcjs"))
;; mptnt1988:
;; Problem with solidity-mode hook, remove it before adding new hook
(setq solidity-mode-hook '())
(add-hook 'solidity-mode-hook 'tnt_solidity-set-checker-executable)

(provide 'tnt_solidity)
;;; tnt_solidity.el ends here
