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
(setq projectile-require-project-root nil)
(defun tnt_projects_neotree-projectile ()
    "Open NeoTree using the git root if any.
If not, open NeoTree with default directory."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (buff-name (buffer-name))
          (file-name (buffer-file-name)))
      (neotree-show)
      (if (neo-global--window-exists-p)
          (progn
            (neotree-dir project-dir)
            (if (string= "." (substring buff-name 0 1))
                (neo-buffer--set-show-hidden-file-p t)
              )
            (neotree-find file-name)))))
(global-set-key (kbd "C-c n s") 'tnt_projects_neotree-projectile)
(global-set-key (kbd "C-c n h") 'neotree-hide)

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
;; TRACE & DEBUG
;;---------------------------
;;--------------------------------------
;; Debug on error
(setq debug-on-error t)

;;--------------------------------------
;; Trace Emacs Lisp
(global-set-key (kbd "C-c C-f") 'find-function-at-point)

(defun tnt_projects_trace-this-func ()
  (interactive)
  (let ((func (function-called-at-point)))
    (trace-function func)
    (message (concat "Tracing " (symbol-name func)))))
(global-set-key (kbd "C-c C-t") 'tnt_projects_trace-this-func)

(defun tnt_projects_untrace-this-func ()
  (interactive)
  (let ((func (function-called-at-point)))
    (untrace-function func)
    (message (concat "Untraced " (symbol-name func)))))
(global-set-key (kbd "C-c C-u") 'tnt_projects_untrace-this-func)

(defun tnt_projects_untrace-all ()
  (interactive)
  (untrace-all)
  (message "Untraced all"))
(global-set-key (kbd "C-c C-M-u") 'tnt_projects_untrace-all)

;;---------------------------
;; OTHER SETTINGS
;;---------------------------
;;--------------------------------------
;; Comment and copy
(defun tnt_projects_comment-copy ()
  (interactive)
  (if (not (use-region-p))
      (comment-dwim nil)
    (save-excursion (kill-ring-save (region-beginning) (region-end) 1))
    (comment-dwim nil)))
(global-set-key "\M-;" 'tnt_projects_comment-copy)

;;--------------------------------------
;; Select phrase at point
(defun tnt_projects_select-phrase ()
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (region-beg (car bounds))
         (region-end (cdr bounds)))
    (goto-char region-beg)
    (set-mark-command nil)
    (goto-char region-end)
    )
  )
(global-set-key (kbd "C-c C-w") 'tnt_projects_select-phrase)

;;--------------------------------------
;; Kill chars of white space class
(defun tnt_projects_kill-whitespace ()
  (interactive)
  (kill-region (point) (progn (skip-chars-forward " \t\r\n") (point)))
  )
(global-set-key (kbd "C-M-z") 'tnt_projects_kill-whitespace)
