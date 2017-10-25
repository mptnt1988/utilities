;; Introduction:
;;   This module includes support functions for all other modules
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (require "tntLib.el")

;; Add repos of packages
(require 'package)
(add-to-list 'package-archives
	     '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install list of packages stored in myPkgs
(defun tntLib_install-myPkgs (&optional postFun)
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)
              (if postFun (funcall postFun))
              ))
        myPkgs))

;; Add multi filename patterns to auto mode list
(defun tntLib_add-files-to-mode (mode lst)
  (dolist (file lst)
    (add-to-list 'auto-mode-alist (cons file mode))))

;; Add add-ins directory to load-path
(add-to-list 'load-path
             (concat (file-name-as-directory dotEmacs-path)
                     "add-ins"))

;; Change color name to hex number
;; Example: mp_colorname2hex("mediumspringgreen")
(defun mp_colorname2hex (name)
  (concat "#"
	  (let (res)
	    (dolist (var (color-values name) res)
	      (setq res (concat res (mp_to-hex var)))))))

(defun mp_to-hex (num)
  (format "%04X" num))

(provide 'tntLib)
