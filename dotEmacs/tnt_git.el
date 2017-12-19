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

;;----------------------------
;; MAGIT
;;----------------------------

(require 'magit)
(global-set-key (kbd "C-c g s") 'magit-status)

;;----------------------------
;; GIT-TIMEMACHINE
;;----------------------------

(require 'git-timemachine)

;;----------------------------
;; SUPPORT FUNCTIONS
;;----------------------------

(defun tnt_git_checkout-gt-revision ()
  "Checkout current git-timemachine revision."
  (interactive)
  (if git-timemachine-revision
      (magit-file-checkout (car git-timemachine-revision)
                           git-timemachine-file)
    (error "Error (mptnt1988): Not in git-timemachine minor mode")))
(global-set-key (kbd "C-c g o") 'tnt_git_checkout-gt-revision)

(provide 'tnt_git)
;;; tnt_git.el ends here
