;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_abbrev.el")

;; tell emacs where to read abbrev definitions from
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")

;; save abbrevs when files are saved
;; you will be asked before the abbreviations are saved
(setq save-abbrevs t)

;; to turn on abbrev-mode globally
(setq-default abbrev-mode t)

;; ;; HOOK FUNCTIONS
(defun dont-insert-expansion-char () t)
(put 'dont-insert-expansion-char 'no-self-insert t)

(defun add-erl-module-name ()
  (backward-char 2)
  (insert (file-name-sans-extension (buffer-name)))
  (move-to-new-line)
  t)
(put 'add-erl-module-name 'no-self-insert t)

(defun move-to-new-line ()
  (end-of-line)
  (newline-and-indent)
  )
(put 'move-to-new-line 'no-self-insert t)

(require 'cl)
(defvar my-abbrev-tables nil)
(defun my-abbrev-hook ()
  (let ((def (assoc (symbol-name last-abbrev) my-abbrev-tables)))
    (when def
      (execute-kbd-macro (cdr def)))
    t))
(put 'my-abbrev-hook 'no-self-insert t)
(defmacro declare-abbrevs (table abbrevs)
  (if (consp table)
      `(progn ,@(loop for tab in table
                      collect `(declare-abbrevs ,tab ,abbrevs)))
    `(progn
       ,@(loop for abbr in abbrevs
               do (when (third abbr)
                    (push (cons (first abbr) (read-kbd-macro (third abbr)))
                          my-abbrev-tables))
               collect `(define-abbrev ,table
                          ,(first abbr) ,(second abbr) ,(and (third abbr)
                                                             ''my-abbrev-hook))))))
(put 'declare-abbrevs 'lisp-indent-function 2)

;; SETTING ABBREVIATION
(clear-abbrev-table global-abbrev-table)

(define-abbrev-table 'global-abbrev-table
  '(
    ;; erlang
    ("mperlm" "-module()." add-erl-module-name)
    ("mperlc" "-compile(export_all)." move-to-new-line)
    ;; scripts
    ("mpube" "#!/usr/bin/env")
    )
  )

(declare-abbrevs (global-abbrev-table)
    (
     ;; emacs lisp
     ("mpeli" "(interactive)" "TAB RET")
     ("mpelm" "(message )" "TAB C-b")
     ;; erlang
     ("mperlio" "io:format(\": ~p~n\", [[{?MODULE, ?LINE}]])," "C-u 3 1 C-b")
     ("mperle" "-export([])." "C-u 3 C-b")
     )
  )
