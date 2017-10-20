;; Introduction:
;;   This module is used for setup and configure packages to manage projects
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_projects.el")

(require 'tntLib)

(setq myPkgs
  '(
    neotree
    projectile
    ack
    undo-tree
    smartparens
    auto-highlight-symbol
    multiple-cursors
    xclip
    ))

(tntLib_install-myPkgs)

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
;; XCLIP
;;---------------------------
;; Note: Linux xclip required
(require 'xclip)
(xclip-mode 1)
(global-set-key (kbd "C-M-y") 'x-clipboard-yank)

;;---------------------------
;; BOOKMARK-LINE
;;---------------------------
(require 'bookmark-line)
(global-set-key (kbd "C-c b a") 'bl_add-line)
(global-set-key (kbd "C-c b r") 'bl_remove-line)
(global-set-key (kbd "C-c b M-r") 'bl_remove-all)
(global-set-key (kbd "C-c b n") 'bl_next-bm)
(global-set-key (kbd "C-c b p") 'bl_previous-bm)
;; (setq mp-bookmark-char "✔")
;; (bl_set-color "color-46")

;;---------------------------
;; TRACE & DEBUG
;;---------------------------
;;--------------------------------------
;; Debug on error
(setq debug-on-error t)

;;--------------------------------------
;; Trace Emacs Lisp
(global-set-key (kbd "C-c C-f") 'find-function-at-point)

(defun tnt_projects_find-function ()
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))
(global-set-key (kbd "C-c f") 'tnt_projects_find-function)

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

;;--------------------------------------
;; Comment and copy
(defun mp-cs-comment-copy ()
  (interactive)
  (if (not (use-region-p))
      (comment-dwim nil)
    (save-excursion (kill-ring-save (region-beginning) (region-end) 1))
    (comment-dwim nil)))
(global-set-key "\M-;" 'mp-cs-comment-copy)

;;--------------------------------------
;; Tab key to indent, untab and delete trailing whitespace
(defun tab-space-indent ()
  (interactive)
  (save-excursion
    (if (use-region-p)
        (let*
            ((firstline (line-number-at-pos (region-beginning)))
             (lastline (line-number-at-pos (region-end)))
             (firstpoint (lambda () (save-excursion
                                      (goto-line firstline)
                                      (line-beginning-position))))
             (lastpoint (lambda () (save-excursion
                                     (goto-line lastline)
                                     (line-end-position)))))
          (delete-trailing-whitespace (funcall firstpoint) (funcall lastpoint))
          (untabify (funcall firstpoint) (funcall lastpoint)))
      (delete-trailing-whitespace (line-beginning-position) (line-end-position))
      (untabify (line-beginning-position) (line-end-position))))
  (indent-for-tab-command))
(global-set-key (kbd "TAB") 'tab-space-indent)

;;--------------------------------------
;; Highlight current line
(global-hl-line-mode 1)

;;--------------------------------------
;; Enable line numbers globally
(global-linum-mode t)

;;--------------------------------------
;; Removing the default start-up buffer
(setq inhibit-startup-message t)

;;--------------------------------------
;; Removing the default start-up screen
'(inhibit-startup-screen t)

;;--------------------------------------
;; Change yes-or-no to y-or-n
(fset `yes-or-no-p `y-or-n-p)

;;--------------------------------------
;; Disable backup
(setq backup-inhibited t)

;;--------------------------------------
;; Disable auto-save
(setq auto-save-default nil)

;;--------------------------------------
;; Match parentheses
(show-paren-mode t)

;;--------------------------------------
;; Show line-number in the mode line
(line-number-mode 1)

;;--------------------------------------
;; Show column-number in the mode line
(column-number-mode 1)

;;--------------------------------------
;; Split horizontally when comparing files by ediff
(setq ediff-split-window-function 'split-window-horizontally)

;;--------------------------------------
;; Line by line scrolling
(setq scroll-step 1)

;;--------------------------------------
;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;;--------------------------------------
;; Set tab width to value
(setq tab-width 4)

;;--------------------------------------
;; Show trailing whitespace
(setq-default show-trailing-whitespace t)
