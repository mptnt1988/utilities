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

(defun my-go-mode-hook ()
  "Define custom hook for go-mode."
  (setq tab-width 4)  ; tab width is 4 spaces
  (setq indent-tabs-mode nil)  ; spaces instead of tab
  )
(add-hook 'go-mode-hook 'my-go-mode-hook)

(require 'go-mode)

(provide 'tnt_go)
;;; tnt_go.el ends here
