;;; tnt_projects --- Manage packages for common projects
;;;---------------------------------------------------------
;;; Commentary:
;;  This module is used for setup and configure packages to
;;  manage projects
;;  Usage:
;;    (require 'tnt_projects)

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

(setq tntLib-myPkgs
  '(
    neotree
    projectile
    ack
    undo-tree
    smartparens
    auto-highlight-symbol
    multiple-cursors
    xclip
    command-log-mode
    iy-go-to-char
    drag-stuff
    flycheck
    yaml-mode
    ))

(tntLib_install-myPkgs)

;;------------------
;; PROJECTILE
;;------------------

(require 'projectile)

(projectile-mode)

;;------------------
;; NEOTREE
;;------------------

(require 'neotree)

(defvar projectile-require-project-root nil)
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
                (neo-buffer--set-show-hidden-file-p t))
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
  (local-set-key (kbd "C-c s s p") 'sp-backward-slurp-sexp))
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

(require 'mp-bookmark-line)

(global-set-key (kbd "C-c b a") 'bl_add-line)
(global-set-key (kbd "C-c b r") 'bl_remove-line)
(global-set-key (kbd "C-c b M-r") 'bl_remove-all)
(global-set-key (kbd "C-c b n") 'bl_next-bm)
(global-set-key (kbd "C-c b p") 'bl_previous-bm)
;; (setq mp-bookmark-char "✔")
;; (bl_set-color "color-46")

;;---------------------------
;; WINDOW SIZE
;;---------------------------

(global-set-key (kbd "C-c <up>") 'enlarge-window)
(global-set-key (kbd "C-c <down>") 'shrink-window)
(global-set-key (kbd "C-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-c <left>") 'shrink-window-horizontally)

;;---------------------------
;; COMMAND LOG MODE
;;---------------------------

(require 'command-log-mode)

;;--------------------------------------
;; Guide:
;;   Toggle the mode:
;;     M-x command-log-mode
;;   Open *command-log* buffer:
;;     M-x clm/open-command-log-buffer
;;   Modify log directory:
;;     (defvar clm/logging-dir "~/log/")
;;   Save to log directory:
;;     M-x clm/save-command-log

;;---------------------------
;; IY-GO-TO-CHAR
;;---------------------------

(require 'iy-go-to-char)

(global-set-key (kbd "M-N") 'iy-go-to-char)
(global-set-key (kbd "M-P") 'iy-go-to-char-backward)
(global-set-key (kbd "M-n") 'iy-go-to-or-up-to-continue)
(global-set-key (kbd "M-p") 'iy-go-to-or-up-to-continue-backward)

;;---------------------------
;; DRAG-STUFF
;;---------------------------

(require 'drag-stuff)

(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(global-set-key (kbd "ESC <up>") 'drag-stuff-up)
(global-set-key (kbd "ESC <down>") 'drag-stuff-down)
(global-set-key (kbd "ESC <left>") 'drag-stuff-left)
(global-set-key (kbd "ESC <right>") 'drag-stuff-right)

;;---------------------------
;; FLYCHECK
;;---------------------------

(require 'flycheck)

(add-hook 'after-init-hook #'global-flycheck-mode)
(setq flycheck-emacs-lisp-load-path 'inherit)

;;---------------------------
;; DIRED MODE
;;---------------------------

(defun tnt_projects_dired-copy-file-here (file)
  "Copy FILE from other directory to here."
  (interactive "fCopy file: ")
  (copy-file file default-directory))
(eval-after-load "dired"
  '(define-key dired-mode-map "Y" 'tnt_projects_dired-copy-file-here))

;;---------------------------
;; TRACE & DEBUG
;;---------------------------

;;--------------------------------------
;; Debug on error
;; (setq debug-on-error t)

;;--------------------------------------
;; Trace Emacs Lisp

(require 'trace)

(global-set-key (kbd "C-c t o") 'find-function-at-point)

(defun tnt_projects_find-function ()
  "Jump to definition of Emacs Lisp function at point."
  (interactive)
  (let ((symb (function-called-at-point)))
    (when symb
      (find-function symb))))
(global-set-key (kbd "C-c t f") 'tnt_projects_find-function)

(defun tnt_projects_trace-this-func ()
  "Trace Emacs Lisp function at point."
  (interactive)
  (let ((func (function-called-at-point)))
    (trace-function func)
    (message (concat "Tracing " (symbol-name func)))))
(global-set-key (kbd "C-c t t") 'tnt_projects_trace-this-func)

(defun tnt_projects_untrace-this-func ()
  "Untrace Emacs Lisp function at point."
  (interactive)
  (let ((func (function-called-at-point)))
    (untrace-function func)
    (message (concat "Untraced " (symbol-name func)))))
(global-set-key (kbd "C-c t u") 'tnt_projects_untrace-this-func)

(defun tnt_projects_untrace-all ()
  "Untrace all Emacs Lisp functions."
  (interactive)
  (untrace-all)
  (message "Untraced all"))
(global-set-key (kbd "C-c t M-u") 'tnt_projects_untrace-all)

;;---------------------------
;; OTHER SETTINGS
;;---------------------------

;;--------------------------------------
;; Comment and copy
(defun tnt_projects_comment-copy ()
  "Comment and also copy selection."
  (interactive)
  (if (not (use-region-p))
      (comment-dwim nil)
    (save-excursion (kill-ring-save (region-beginning) (region-end) 1))
    (comment-dwim nil)))
(global-set-key "\M-;" 'tnt_projects_comment-copy)

;;--------------------------------------
;; Select phrase at point
(defun tnt_projects_select-phrase ()
  "Select phrase at point."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'symbol))
         (region-beg (car bounds))
         (region-end (cdr bounds)))
    (goto-char region-beg)
    (set-mark-command nil)
    (goto-char region-end)))
(global-set-key (kbd "C-c C-w") 'tnt_projects_select-phrase)

;;--------------------------------------
;; Kill chars of white space class
(defun tnt_projects_kill-whitespace ()
  "Kill all whitespaces."
  (interactive)
  (kill-region (point) (progn (skip-chars-forward " \t\r\n") (point))))
(global-set-key (kbd "C-M-z") 'tnt_projects_kill-whitespace)

;;--------------------------------------
;; Tab key to indent, untab and delete trailing whitespace
(defun tnt_projects_tab-space-indent ()
  "Press tab key to indent, untab and delete trailing whitespace."
  (interactive)
  (save-excursion
    (if (use-region-p)
        (let*
            ((firstline (line-number-at-pos (region-beginning)))
             (lastline (line-number-at-pos (region-end)))
             (firstpoint (lambda () (save-excursion
                                      (forward-line (- firstline
                                                       (line-number-at-pos)))
                                      (line-beginning-position))))
             (lastpoint (lambda () (save-excursion
                                     (forward-line (- lastline
                                                      (line-number-at-pos)))
                                     (line-end-position)))))
          (delete-trailing-whitespace (funcall firstpoint)
                                      (funcall lastpoint))
          (untabify (funcall firstpoint) (funcall lastpoint)))
      (delete-trailing-whitespace (line-beginning-position)
                                  (line-end-position))
      (untabify (line-beginning-position)
                (line-end-position))))
  (indent-for-tab-command))
(global-set-key (kbd "TAB") 'tnt_projects_tab-space-indent)

;;--------------------------------------
;; Duplicate current line
(defun tnt_projects_duplicate-line ()
  "Duplicate current line."
  (interactive)
  (let ((temp (get-register 57)))
    (copy-to-register 57
                      (line-beginning-position)
                      (line-end-position))
    (move-end-of-line 1)
    (newline)
    (insert-register 57)
    (set-register 57 temp)
    ))
(global-set-key (kbd "C-c d") 'tnt_projects_duplicate-line)

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
(defvar ediff-split-window-function 'split-window-horizontally)

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

;; *mptnt1988*:
;;   Currently, neotree warning cannot be disabled
;;   due to its own fault
(provide 'tnt_projects)
;;; tnt_projects.el ends here
