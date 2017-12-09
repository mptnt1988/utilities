;;; tnt_erlang --- Module for Erlang
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_erlang)

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

;;------------------
;; NORMAL ERLANG
;;------------------

(defun chomp (s)
  "Support getting Erlang root from S string."
  (car (last (butlast (split-string s "[\r\n]")))))

(defun erl-root ()
  "Get Erlang root."
  (chomp
   (shell-command-to-string
    "sed '/^ROOTDIR=/{s///; s/\"//g; q;}; d' `which erl`")))

(defun set-erlang-dir (dir)
  "Set Erlang root to DIR."
  (let ((bin-dir (expand-file-name "bin" dir))
        (tools-dirs (file-expand-wildcards
                     (concat dir "/lib/tools-*/emacs"))))
    (when tools-dirs
      (add-to-list 'load-path (car tools-dirs))
      (add-to-list 'exec-path bin-dir)
      ;; auto erlang mode for some files
      (tntLib_add-files-to-mode 'erlang-mode
                                (list "\\.erlcfg$"
                                      "\\rebar.config$"))
      (defvar erlang-electric-commands
        '(erlang-electric-comma
          erlang-electric-semicolon
          erlang-electric-gt
          erlang-electric-newline))
      (defvar erlang-root-dir dir)
      (require 'erlang-start))))

(set-erlang-dir (erl-root))

(defun my-erlang-mode-hook ()
  "Define custom hook for erlang-mode."
  ;; when starting an Erlang shell in Emacs, default node name
  (defvar inferior-erlang-machine-options '("-sname" "emacs"))
  ;; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  ;; customize keys
  (local-set-key [return] 'newline-and-indent))
; Some Erlang customizations
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;;------------------
;; EDTS
;;------------------

;; tntLib required
(setq tntLib-myPkgs
  '(
    edts
    ))

(defun tnt_erlang_auto-make-edts ()
  "Auto make EDTS."
  (message "Auto make edts by mptnt1988")
  (let*
      ((tntcd "cd ~/.emacs.d/elpa/edts-*")
       (tntmake "make"))
    (call-process-shell-command
     (concat tntcd " && "
             tntmake))))
(tntLib_install-myPkgs 'tnt_erlang_auto-make-edts)

(defun my-edts-after-init-hook ()
  "Erlang mode hook for EDTS."
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

(provide 'tnt_erlang)
;;; tnt_erlang.el ends here
