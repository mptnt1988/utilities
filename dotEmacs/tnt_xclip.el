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
    xclip
    ))

(tntLib_install-myPkgs)

;; Note: Linux xclip required
(require 'xclip)

(xclip-mode 1)
(global-set-key (kbd "C-M-y") 'x-clipboard-yank)

(provide 'tnt_xclip)
;;; tnt_xclip.el ends here
