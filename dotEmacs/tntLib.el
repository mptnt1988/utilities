;;; tntLib --- Library module
;;;---------------------------------------------------------
;;; Commentary:
;;  This includes support functions for all other modules
;;  Usage:
;;    (require 'tntLib)

;;;---------------------------------------------------------
;;; Code:

;; Add repos of packages
(require 'package)
(add-to-list 'package-archives
	     '("elpa" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

;; Install list of packages stored in tntLib-myPkgs
(defvar tntLib-myPkgs '()
  "Packages to be installed.
Define this variable and invoke function tntLib_install-myPkgs")

(defun tntLib_install-myPkgs (&optional postFun)
  "Install packages if they have not been installed yet.
The packages must be specified in tntLib-myPkgs variable.
POSTFUN if exists will be run after installation."
  (mapc #'(lambda (package)
            (unless (package-installed-p package)
              (package-install package)
              (if postFun (funcall postFun))))
        tntLib-myPkgs))

(defun tntLib_add-files-to-mode (mode lst)
  "Add multi filename patterns to auto mode list.
MODE is a major mode.  LST is list of file extensions."
  (dolist (file lst)
    (add-to-list 'auto-mode-alist (cons file mode))))

(defun mp_colorname2hex (name)
  "Change color NAME to hex number.
Example: mp_colorname2hex(\"mediumspringgreen\")"
  (concat "#"
	  (let (res)
	    (dolist (var (color-values name) res)
	      (setq res (concat res (mp_to-hex var)))))))

(defun mp_to-hex (num)
  "Change a number NUM to hex format."
  (format "%04X" num))

(provide 'tntLib)
;;; tntLib.el ends here
