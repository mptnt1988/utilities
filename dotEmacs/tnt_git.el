;; Introduction:
;;   This module is used for setup and configure Git usage in Emacs
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_git.el")

(defvar myPkgs_git
  '(magit
    git-timemachine
    diff-hl
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPkgs_git)
