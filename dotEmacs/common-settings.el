;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Copy from this file to ~/.emacs    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--------------------------------------
;; Unicode
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;--------------------------------------
;; Disable backup
(setq backup-inhibited t)

;;--------------------------------------
;; Disable auto save
(setq auto-save-default nil)

;;--------------------------------------
;; Use spaces instead of tab
(setq-default indent-tabs-mode nil)

;;--------------------------------------
;; Set tab width to value
(setq tab-width 4)

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
;; Color for selected text
(set-face-background 'region "black")

;;--------------------------------------
;; Auto-save
(setq auto-save-default nil)

;;--------------------------------------
;; Set keybindings
(global-set-key "\C-l" 'goto-line)
(global-set-key "\C-r" 'query-replace)
(global-set-key [f5] 'my-next-long-line)
(global-set-key [f6] 'erl-node-choosing)
(global-set-key [f7] 'default-bookmark-load)
(global-set-key [f8] 'bookmark-save)
(global-set-key [f9] 'bookmark-delete)
(global-set-key [f10] 'revert-all-buffers)
(global-set-key (kbd "C-x M-b") 'ibuffer)
;; define arrow-keys for no-window-emacs
(define-key input-decode-map "\e[1;5A" [C-up])
(define-key input-decode-map "\e[1;5B" [C-down])
(define-key input-decode-map "\M-[1;5C" [C-right])
(define-key input-decode-map "\M-[1;5D" [C-left])

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
;; Enable clipboard
(setq x-select-enable-clipboard t)

;;--------------------------------------
;; Split horizontally when comparing files by ediff
(setq ediff-split-window-function 'split-window-horizontally)

;;--------------------------------------
;; Start in full-screen mode
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
	    		 '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)
(if (display-graphic-p)
    (toggle-fullscreen))

;;--------------------------------------
;; Load certain packages
(load "tnt_package.el")
(defvar myPackages
  '(better-defaults
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

(load-theme 'material t) ; load material theme

;;--------------------------------------
;; Set frame size according to resolution
(defun set-frame-size-according-to-resolution ()
  (interactive)
  (if window-system
  (progn
    ;; use 120 char wide window for largeish displays
    ;; and smaller 80 column windows for smaller displays
    ;; pick whatever numbers make sense for you
    (add-to-list 'default-frame-alist (cons 'width 164))
    ;; for the height, subtract a couple hundred pixels
    ;; from the screen height (for panels, menubars and
    ;; whatnot), then divide by the height of a char to
    ;; get the height we want
    (add-to-list 'default-frame-alist
         (cons 'height (/ (- (x-display-pixel-height) 50)
                             (frame-char-height)))))))
(set-frame-size-according-to-resolution)

;;--------------------------------------
;; Find 80 char long lines
(defun my-next-long-line (arg)
  (set-fill-column 80)
  "Move to the ARGth next long line greater than `fill-column'."
  (interactive "p")
  (or arg (setq arg 1))
  (let ((opoint (point))
        (line-length 0))
    ;; global-variable: fill-column
    (while (and (<= line-length fill-column)
                (zerop (forward-line (if (< arg 0) -1 1))))
      (setq line-length (save-excursion
                          (end-of-line)
                          (current-column))))
    ;; Stop, end of buffer reached.
    (if (> line-length fill-column)
        (if (> arg 1)
            (my-next-long-line (1- arg))
          (if (< arg -1)
              (my-next-long-line (1+ arg))
            (message (format "Long line of %d columns found"
                             line-length))))
      (goto-char opoint)
      (message "Long line not found"))))

;;--------------------------------------
;; Trailing whitespace
;; Mark the trailing whitespace clearly
;; If you open a file which has a lot of trailing whitespaces
;; you can use Meta (alt x) delete-trailing-whitespace
(setq-default show-trailing-whitespace t)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;--------------------------------------
;; Revert all buffers
(defun revert-all-buffers ()
    "Refreshes all open buffers from their respective files."
    (interactive)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (buffer-file-name) (file-exists-p (buffer-file-name)) (not (buffer-modified-p)))
          (revert-buffer t t t) )))
    (message "Refreshed open files.") )
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

;;--------------------------------------
;; diff-hl configuration for console mode
(diff-hl-mode)
(diff-hl-flydiff-mode)
(diff-hl-margin-mode)

;;--------------------------------------
;; Example on adding a hook
;; (erlang-mode hook)
(add-hook 'erlang-mode-hook '(lambda() (setq indent-tabs-mode nil)))

;;--------------------------------------
;; Highlight current line
(global-hl-line-mode 1)

;;--------------------------------------
;; Change default welcome message to "xyz"
(defun display-startup-echo-area-message ()
  (message "xyz"))
;; or remove it
(defun display-startup-echo-area-message ())

;;--------------------------------------
;; Line by line scrolling
(setq scroll-step 1)
