;; Introduction:
;;   This module is used for setup and configure both Elpy and Jedi for Python
;; Note 1:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_python.el")
;; Note 2:
;;   By default, Elpy uses python 2
;;   To switch it to python 3, insert this in .emacs:
;;     (tnt_python_setup-python3)

(require 'tntLib)

(setq myPkgs
  '(virtualenv
    jedi
    elpy
    ))

(tntLib_install-myPkgs)

;;;;;;;;;;;;;;;;;;;;;;
;;; ELPY CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;

;; Enable Python package
(elpy-enable)

;; Set Elpy RPC backend to Jedi
(setq elpy-rpc-backend "jedi")

;; Remap jumping-back-from-definition to "M-," instead of "M-*"
(define-key elpy-mode-map (kbd "M-,") 'pop-tag-mark)

;; Add PIP packages' bin path to exec-path
(add-to-list 'exec-path
             (substitute-in-file-name "/home/$USER/.local/bin"))

;;;;;;;;;;;;;;;;;;;;;;
;;; JEDI CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;

;;------------
;; Basic setup
;;------------

(require 'jedi)

;; Hook up to auto-complete
(add-to-list 'ac-sources 'ac-source-jedi-direct)
;; Enable for python-mode
(add-hook 'python-mode-hook 'jedi:setup)

;;-------------------
;; Jedi configuration
;;-------------------

;; Allow virtualenv
(defvar jedi-config:with-virtualenv nil
  "Set to non-nil to point to a particular virtualenv.")

;; Variables to help find the project root
(defvar jedi-config:vcs-root-sentinel ".git")
(defvar jedi-config:python-module-sentinel "__init__.py")

;; Function to find project root given a buffer
(defun get-project-root (buf repo-type init-file)
  (vc-find-root (expand-file-name (buffer-file-name buf)) repo-type))
(defvar jedi-config:find-root-function 'get-project-root)

;; This is called on initialization
(defun current-buffer-project-root ()
  (funcall jedi-config:find-root-function
	   (current-buffer)
	   jedi-config:vcs-root-sentinel
	   jedi-config:python-module-sentinel))

(defun jedi-config:setup-server-args ()
  ;; Little helper macro
  (defmacro add-args (arg-list arg-name arg-value)
    `(setq ,arg-list (append ,arg-list (list ,arg-name ,arg-value))))

  (let ((project-root (current-buffer-project-root)))
    ;; Variable for this buffer only
    (make-local-variable 'jedi:server-args)

    ;; And set our variables
    (when project-root
      (add-args jedi:server-args "--sys-path" project-root))

    ;; Allow virtualenv
    (when project-root
      (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv)
      )))

;; Tuan: Uncomment this for OS X
;; (defvar jedi-config:use-system-python t)
;; (defun jedi-config:set-python-executable ()
;;   (set-exec-path-from-shell-PATH)
;;   (make-local-variable 'jedi:server-command)
;;   (set 'jedi:server-command
;;        (list (executable-find "python")
;; 	     (cadr default-jedi-server-command))))

;; Putting everything together
(add-hook 'python-mode-hook
	  'jedi-config:setup-server-args)

;; Tuan: Uncomment this for OS X
;; (when jedi-config:use-system-python
;;   (add-hook 'python-mode-hook
;; 	    'jedi-config:set-python-executable))

;;-------------
;; Key bindings
;;-------------
;; Tuan: Below key bindings for "M-." and "M-," will only be active when
;;       elpy-mode is disabled. Otherwise, above Elpy setup takes place.
(defun jedi-config:setup-keys()
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-?") 'jedi:show-doc)
  (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)

;;------
;; Misc.
;;------
;; Set python-indent-guess-indent-offset to nil
(setq python-indent-guess-indent-offset nil)

;; Small hack to never show in-function call automatically
(setq jedi:get-in-function-call-delay 10000000)

;; Complete when typing a dot
(setq jedi:complete-on-dot t)

;; Function to setup python3
(defun tnt_python_setup-python3 ()
  (setq elpy-rpc-python-command "python3")
  (pop jedi:install-server--command)
  (push "pip3" jedi:install-server--command)
  (setq python-shell-interpreter "python3")
  (setq elpy-test-discover-runner-command '("python3" "-m" "unittest"))
  (message "Python mode is using Python 3."))

;; Wrapper to pyvenv-activate a virtualenv
(defun tnt_pyvenv_activate (directory)
  (interactive "DActivate venv: ")
  (pyvenv-activate directory)
  (message "mptnt1988: Installing packages to the virtualenv directory...")
  (shell-command
   (concat
    "cd " directory " && "
    "pip3 install jedi epc flake8 importmagic autopep8 yapf virtualenv"))
  (message "mptnt1988: Installation done!!!"))

(provide 'tnt_python)
;;; tnt_python.el ends here
