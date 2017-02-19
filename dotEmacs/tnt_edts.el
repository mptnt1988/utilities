;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_edts.el")

;;------------ start part 1 -------------
;; (require 'package)
;; Add repositories for package.el
;; (setq package-archives '(("elpa"      . "http://elpa.gnu.org/packages/")
;;                          ("melpa"     . "http://melpa.milkbox.net/packages/")
;;                          ("marmalade" . "http://marmalade-repo.org/packages/")))
;; (package-initialize)
;;------------ end part 1 ---------------

;;------------ start part 2 -------------
;; Start EDTS after Emacs is initialized
(defun my-edts-after-init-hook ()
  (unless (ignore-errors (require 'edts-start))
    (warn "EDTS is not installed in this environment!")))
(add-hook 'after-init-hook 'my-edts-after-init-hook)
;;------------ end part 2 ---------------
