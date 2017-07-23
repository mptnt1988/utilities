;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_web-mode.el")

(defvar myPkgs_web
  '(web-mode
    php-mode
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (package-install package)))
      myPkgs_web)

(require 'web-mode)
