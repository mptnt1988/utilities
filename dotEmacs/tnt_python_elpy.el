;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_python_elpy.el")
(defvar myPkgs_python_elpy
  '(
    elpy
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPkgs_python_elpy)

;;;;;;;;;;;;;;
;;; CUSTOM ;;;
;;;;;;;;;;;;;;

;; Enable Python package
(elpy-enable)

;; Set Elpy RPC backend to Jedi
(setq elpy-rpc-backend "jedi")

;; Remap jumping-back-from-definition to "M-," instead of "M-*"
(define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)

;; Add PIP packages' bin path to exec-path
(add-to-list 'exec-path
             (substitute-in-file-name "/home/$USER/.local/bin")
             )
