(defvar myPkgs_python
  '(virtualenv
    jedi
    elpy
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPkgs_python)

;;;;;;;;;;;;;;;;;;;;;;
;;; ELPY CONFIGURATION
;;;;;;;;;;;;;;;;;;;;;;

;; Enable Python package
(elpy-enable)

;; Set Elpy RPC backend to Jedi
(setq elpy-rpc-backend "jedi")

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

;; Tuan: Uncomment this to allow virtualenv
;; (defvar jedi-config:with-virtualenv nil
;;   "Set to non-nil to point to a particular virtualenv.")

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

    ;; Tuan: Uncomment this to allow virtualenv
    ;; (when project-root
    ;;   (add-args jedi:server-args "--virtual-env" jedi-config:with-virtualenv)
    ;;   )
    ))

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
(defun jedi-config:setup-keys()
  (local-set-key (kbd "M-.") 'jedi:goto-definition)
  (local-set-key (kbd "M-,") 'jedi:goto-definition-pop-marker)
  (local-set-key (kbd "M-?") 'jedi:show-doc)
  (local-set-key (kbd "M-/") 'jedi:get-in-function-call))
(add-hook 'python-mode-hook 'jedi-config:setup-keys)

;;------
;; Misc.
;;------

;; Small hack to never show in-function call automatically
(setq jedi:get-in-function-call-delay 10000000)

;; Complete when typing a dot
(setq jedi:complete-on-dot t)
