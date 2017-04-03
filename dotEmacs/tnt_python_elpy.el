;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_python_elpy.el")
(defvar myPackages
  '(elpy ; Python package
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;;;;;;;;;;;;;;
;;; CUSTOM ;;;
;;;;;;;;;;;;;;

;; Enable Python package
(elpy-enable)

;; Set Elpy RPC backend to Jedi
(setq elpy-rpc-backend "jedi")

;; Remap jumping-back-from-definition to "M-," instead of "M-*"
(define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)

;; Add path to flake8 to exec-path (if installed as an Emacs package)
;; (add-to-list 'exec-path "/home/tuantran/.local/bin")
