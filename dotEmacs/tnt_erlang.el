;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_erlang.el")

(require 'tntLib)

;;------------------
;; NORMAL ERLANG
;;------------------

(defun chomp (s)
  (car (last (butlast (split-string s "[\r\n]")))))

(defun erl-root ()
  (chomp
   (shell-command-to-string
    "sed '/^ROOTDIR=/{s///; s/\"//g; q;}; d' `which erl`")))

(defun set-erlang-dir (dir)
  (let ((bin-dir (expand-file-name "bin" dir))
        (tools-dirs (file-expand-wildcards
                     (concat dir "/lib/tools-*/emacs"))))
    (when tools-dirs
      (add-to-list 'load-path (car tools-dirs))
      (add-to-list 'exec-path bin-dir)
      ;; auto erlang mode for some files
      (tntLib_add-files-to-mode 'erlang-mode
                                (list "\\.erlcfg$"
                                      "\\rebar.config$"
                                      ))
      (defvar erlang-electric-commands
        '(erlang-electric-comma
          erlang-electric-semicolon
          erlang-electric-gt
          erlang-electric-newline))
      (setq erlang-root-dir dir)
      (require 'erlang-start))))

(set-erlang-dir (erl-root))

(defun my-erlang-mode-hook ()
  ;; when starting an Erlang shell in Emacs, default node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  ;; customize keys
  (local-set-key [return] 'newline-and-indent)
  )
; Some Erlang customizations
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;;------------------
;; EDTS
;;------------------

;; tntLib required
(setq myPkgs
  '(
    edts
    ))

(defun tnt_erlang_auto-make-edts ()
  (message "Auto make edts by mptnt1988")
  (let*
      ((tntcd "cd ~/.emacs.d/elpa/edts-*")
       (tntmake "make")
       )
    (call-process-shell-command
     (concat tntcd " && "
             tntmake))))
(tntLib_install-myPkgs 'tnt_erlang_auto-make-edts)

(defun my-edts-after-init-hook ()
  (unless (ignore-errors (require 'edts-start))
    (warn "EDTS is not installed in this environment!")))
(add-hook 'erlang-mode-hook 'my-edts-after-init-hook)

;;------------------
;; DISTEL
;;------------------

;; TODO: Consider using distel

;; (require 'distel)
;; (distel-setup)
;; (put 'narrow-to-region 'disabled nil)

;;------------------
;; FLYMAKE
;;------------------

;; TODO: Consider using flymake

;; ;; Usage:
;; ;;   Use command flymake-mode to toggle the buffer's mode
;; ;; Notes:
;; ;;   0.Add path to this file to 'load-path
;; ;;     (add-to-list 'load-path "/path/to/this/file")
;; ;;     then
;; ;;     (load "tnt_flymake-erlang.el")
;; ;;   1.Add path to flymake.el to 'load-path
;; ;;     (add-to-list 'load-path "/path/to/flymake.el")
;; ;;   2.Set path-to-erl-flymake-escript global variable:
;; ;;     (setq path-to-erl-flymake-escript "/path/to/escript")
;; (require 'flymake)
;; (defun flymake-erlang-init ()
;;   (let* ((temp-file (flymake-init-create-temp-buffer-copy
;; 		     'flymake-create-temp-inplace))
;; 	 (local-file (file-relative-name temp-file
;; 					 (file-name-directory buffer-file-name))))
;;     (list path-to-erl-flymake-escript (list local-file))))
;; (add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
