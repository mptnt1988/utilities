;; Introduction:
;;   This module is used for setup and configure packages to manage projects
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_projects.el")

(defvar myPkgs_projects
  '(
    neotree
    projectile
    ack
    undo-tree
    smartparens
    auto-highlight-symbol
    multiple-cursors
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPkgs_projects)

;;------------------
;; PROJECTILE
;;------------------
(require 'projectile)
(projectile-global-mode)

;;------------------
;; NEOTREE
;;------------------
(require 'neotree)
(defun neotree-projectile ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

;;------------------
;; UNDO-TREE
;;------------------
(require 'undo-tree)
(global-undo-tree-mode)

;;------------------
;; SMARTPARENS MODE
;;------------------
(require 'smartparens)
(smartparens-global-mode)
(require 'smartparens-config)

(defun my-sp-hook()
  (local-set-key (kbd "C-c s m n") 'sp-forward-sexp)
  (local-set-key (kbd "C-c s m p") 'sp-backward-sexp)
  (local-set-key (kbd "C-c s u n") 'sp-unwrap-sexp)
  (local-set-key (kbd "C-c s u p") 'sp-backward-unwrap-sexp)
  (local-set-key (kbd "C-c s b n") 'sp-forward-barf-sexp)
  (local-set-key (kbd "C-c s b p") 'sp-backward-barf-sexp)
  (local-set-key (kbd "C-c s s n") 'sp-forward-slurp-sexp)
  (local-set-key (kbd "C-c s s p") 'sp-backward-slurp-sexp)
  )
(add-hook 'smartparens-mode-hook 'my-sp-hook)

;;---------------------------
;; AUTO HIGHLIGHT SYMBOL MODE
;;---------------------------
(require 'auto-highlight-symbol)
(global-auto-highlight-symbol-mode)

;;---------------------------
;; MULTIPLE-CURSORS
;;---------------------------
(require 'multiple-cursors)
(global-set-key (kbd "C-c m C-c") 'mc/edit-lines)
(global-set-key (kbd "C-c m m >") 'mc/mark-next-like-this)
(global-set-key (kbd "C-c m m <") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m u >") 'mc/unmark-next-like-this)
(global-set-key (kbd "C-c m u <") 'mc/unmark-previous-like-this)

;;---------------------------
;; SET KEYS IN USE
;;---------------------------
(global-set-key (kbd "C-c C-f") 'find-function-at-point)

(defun tnt_projects_trace-this-func ()
  (interactive)
  (trace-function (function-called-at-point))
  )
(global-set-key (kbd "C-c C-t") 'tnt_projects_trace-this-func)

(defun tnt_projects_untrace-this-func ()
  (interactive)
  (untrace-function (function-called-at-point))
  )
(global-set-key (kbd "C-c C-u") 'tnt_projects_untrace-this-func)

(global-set-key (kbd "C-c C-M-u") 'untrace-all)
