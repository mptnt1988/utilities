;;; tnt_go --- Setup and configure Emacs Go
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_go)

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

(setq tntLib-myPkgs
  '(
    go-mode
    ))

(tntLib_install-myPkgs)

(require 'go-mode)

(provide 'tnt_go)
;;; tnt_go.el ends here
