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

;; setting abbreviation
(clear-abbrev-table global-abbrev-table)
(define-abbrev-table 'global-abbrev-table
  '(
    ("mpube" "#!/usr/bin/env")
    ("mperlio" "io:format(\"MPTrace: ~p~n\", [[{?MODULE, ?LINE}]]),")
    )
  )
