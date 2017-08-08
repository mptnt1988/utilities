;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_erlang.el")

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
      (defvar erlang-electric-commands
        '(erlang-electric-comma
          erlang-electric-semicolon
          erlang-electric-gt
          erlang-electric-newline))
      (setq erlang-root-dir dir)
      (require 'erlang-start))))

(set-erlang-dir (erl-root))

(defun my-erlang-mode-hook ()
  ; when starting an Erlang shell in Emacs, default node name
  (setq inferior-erlang-machine-options '("-sname" "emacs"))
  ; add Erlang functions to an imenu menu
  (imenu-add-to-menubar "imenu")
  ; customize keys
  (local-set-key [return] 'newline-and-indent)
  )
; Some Erlang customizations
(add-hook 'erlang-mode-hook 'my-erlang-mode-hook)

;;------------------
;; EDTS
;;------------------
;;------------ start part 1 -------------
(require 'package)
;; Add repositories for package.el
(setq package-archives '(("elpa"      . "http://elpa.gnu.org/packages/")
                         ("melpa"     . "http://melpa.milkbox.net/packages/")
                         ;; ("marmalade" . "http://marmalade-repo.org/packages/")
                         ))
(package-initialize)

(defvar myPkgs_edts
  '(
    edts
    ))

(mapc #'(lambda (package)
          (unless (package-installed-p package)
            (progn
              (package-install package)
              (message "Auto make edts by mptnt1988")
              (let*
                  ((tntcd "cd ~/.emacs.d/elpa/edts-*")
                   (tntmake "make")
                   )
                (shell-command
                 (concat tntcd " && "
                         tntmake))
                )
              )
            )
          )
      myPkgs_edts)
;;------------ end part 1 ---------------

;;------------ start part 2 -------------
;; Start EDTS after Emacs is initialized
(defun my-edts-after-init-hook ()
  (unless (ignore-errors (require 'edts-start))
    (warn "EDTS is not installed in this environment!")))
;; (add-hook 'after-init-hook 'my-edts-after-init-hook)
(add-hook 'erlang-mode-hook 'my-edts-after-init-hook)
;;------------ end part 2 ---------------
