;;; tnt_git --- Setup and configure Git usage in Emacs
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_git)

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

(setq tntLib-myPkgs
  '(
    magit
    git-timemachine
    diff-hl
    ))

(tntLib_install-myPkgs)

(provide 'tnt_git)
;;; tnt_git.el ends here
