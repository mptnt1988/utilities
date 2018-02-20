;;; tnt_abbrev --- Abbreviation module
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_abbrev)

;;;---------------------------------------------------------
;;; Code:

;; tell emacs where to read abbrev definitions from
(setq abbrev-file-name
      "~/.emacs.d/abbrev_defs")

;; save abbrevs when files are saved
;; you will be asked before the abbreviations are saved
(setq save-abbrevs t)

;; to turn on abbrev-mode globally
(setq-default abbrev-mode t)

;;---------------
;; HOOK FUNCTIONS
;;---------------

;; common use
(defun dont-insert-expansion-char ()
  "This is example of a hook function."
  t)
(put 'dont-insert-expansion-char 'no-self-insert t)

(defun tnt_abbrev_move-to-new-line ()
  "Move to new line and indent."
  (end-of-line)
  (newline-and-indent))
(put 'tnt_abbrev_move-to-new-line 'no-self-insert t)

;; Emacs Lisp
(defun tnt_abbrev_emacs-lisp-add-head ()
  "Add head of Emacs module."
  (backward-char 5)
  (insert (file-name-sans-extension (buffer-name)))
  (move-end-of-line nil)
  t)
(put 'tnt_abbrev_emacs-lisp-add-head 'no-self-insert t)

(defun tnt_abbrev_emacs-lisp-add-cm ()
  "Add Commentary comment part of Emacs module."
  (save-excursion
    (backward-char 17)
    (insert-char ?- 57))
  t)
(put 'tnt_abbrev_emacs-lisp-add-cm 'no-self-insert t)

(defun tnt_abbrev_emacs-lisp-add-code ()
  "Add Code comment part of Emacs module."
  (save-excursion
    (backward-char 11)
    (insert-char ?- 57))
  t)
(put 'tnt_abbrev_emacs-lisp-add-code 'no-self-insert t)

(defun tnt_abbrev_emacs-lisp-add-tail ()
  "Add tail of Emacs module."
  (backward-char 19)
  (let ((module (file-name-sans-extension (buffer-name))))
    (insert module)
    (forward-char 6)
    (insert module))
  (move-end-of-line nil)
  t)
(put 'tnt_abbrev_emacs-lisp-add-tail 'no-self-insert t)

(defun tnt_abbrev_emacs-lisp-add-skeleton ()
  "Add skeleton of Emacs module."
  (let ((module (file-name-sans-extension (buffer-name))))
    (goto-char (point-min))
    (forward-char 4)
    (insert module)
    (forward-char 5)
    (save-excursion
      (forward-char 4)
      (insert-char ?- 57)
      (forward-char 21)
      (insert-char ?- 57)
      (forward-char 22)
      (insert module)
      (forward-char 6)
      (insert module)))
  t)
(put 'tnt_abbrev_emacs-lisp-add-skeleton 'no-self-insert t)

;; Erlang
(defun tnt_abbrev_erl-add-module-name ()
  "Add Erlang module name."
  (backward-char 2)
  (insert (file-name-sans-extension (buffer-name)))
  (tnt_abbrev_move-to-new-line)
  t)
(put 'tnt_abbrev_erl-add-module-name 'no-self-insert t)

(defun tnt_abbrev_erl-get-record-name ()
  "Get record name."
  (backward-char 6)
  (insert (read-string "Record name: "))
  (forward-char 3)
  t)
(put 'tnt_abbrev_erl-get-record-name 'no-self-insert t)

;; JavaScript
(defun tnt_abbrev_js-get-module-name ()
  "Get name of NodeJS module."
  (let ((module (read-string "Module name: ")))
    (backward-char 16)
    (insert module)
    (save-excursion
      (forward-char 12)
      (insert module)))
  t)
(put 'tnt_abbrev_js-get-module-name 'no-self-insert t)

;; important support functions
;; *mptnt1988*:
;;   Currently not able to remove cl related warning
(require 'cl)
(defvar my-abbrev-tables nil)
(defun my-abbrev-hook ()
  "Doc string for my-abbrev-hook."
  (let ((def (assoc (symbol-name last-abbrev) my-abbrev-tables)))
    (when def
      (execute-kbd-macro (cdr def)))
    t))
(put 'my-abbrev-hook 'no-self-insert t)
(defmacro declare-abbrevs (table abbrevs)
  "Doc strong for declar-abbrevs with params TABLE and ABBREVS."
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
    ;; emacs lisp
    ("mpelhd" ";;;  --- " tnt_abbrev_emacs-lisp-add-head)
    ("mpelcm" ";;;\n;;; Commentary:\n" tnt_abbrev_emacs-lisp-add-cm)
    ("mpelco" ";;;\n;;; Code:\n" tnt_abbrev_emacs-lisp-add-code)
    ("mpelsk" ";;;  --- \n;;;\n;;; Commentary:\n\n;;;\n;;; Code:\n\n(provide ')\n;;; .el ends here" tnt_abbrev_emacs-lisp-add-skeleton)
    ("mpeltl" "(provide ')\n;;; .el ends here" tnt_abbrev_emacs-lisp-add-tail)
    ;; erlang
    ("mperlc" "-compile(export_all)." tnt_abbrev_move-to-new-line)
    ("mperlm" "-module()." tnt_abbrev_erl-add-module-name)
    ("mperlr" "-record(, {})." tnt_abbrev_erl-get-record-name)
    ;; javascript
    ("mpjsr" "var  = require('');\n" tnt_abbrev_js-get-module-name)
    ;; scripts
    ("mpube" "#!/usr/bin/env")))

(declare-abbrevs (global-abbrev-table)
    (
     ;; emacs lisp
     ("mpeli" "(interactive)" "TAB RET")
     ("mpeld" "(defun  ()\n  \n)" "C-u 8 C-b")
     ("mpelm" "(message )" "TAB C-b")
     ("mpelt" "(defun  ()\n  (interactive)\n  \n)" "C-u 2 4 C-b")
     ;; erlang
     ("mperlb" "-behaviour()." "C-u 2 C-b")
     ("mperld" "-define()." "C-u 2 C-b")
     ("mperle" "-export([])." "C-u 3 C-b")
     ("mperli" "-include(\"\")." "C-u 3 C-b")
     ("mperlil" "-include_lib(\"\")." "C-u 3 C-b")
     ("mperlio" "io:format(\": ~p~n\", [[{?MODULE, ?LINE}]])," "C-u 3 1 C-b")
     ;; javascript
     ("mpjscl" "console.log();" "C-b C-b")
     ;; python
     ("mppy3" "#!/usr/bin/env python3" "RET")
     ("mppyi" "def __init__(self):" "C-u 2 C-b")
     ("mppym" "if __name__ == '__main__':" "RET")
     ("mppynp" "import numpy as np" "RET")
     ("mppypd" "import pandas as pd" "RET")
     ("mppyplt" "import matplotlib.pyplot as plt" "RET")
     ("mppysns" "import seaborn as sns" "RET")))

(provide 'tnt_abbrev)
;;; tnt_abbrev.el ends here
