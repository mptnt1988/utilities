;;; --- mp-cmdlist.el ---
;;; Conveniently running Linux async commands in Emacs

;;====================================================================
;; Author: Tuan N. Tran <mptnt1988@gmail.com>
;; Created: 07 Nov 2016
;; Version: 1.0

;;====================================================================
;; REVISION:
;; 1.0         Firstly created

;;====================================================================
;; GUIDE:
;; - Public APIs:
;;    + cmdlist-add
;;    + cmdlist-show
;;    + cmdlist-run
;;    + cmdlist-remove
;;    + cmdlist-replace
;;    + cmdlist-clear
;; - Usages:
;;    + Add new command into cmdlist by cmdlist-add function
;;    + Show cmdlist - commands with their positions - by cmdlist-show
;;      function
;;    + Run Linux async command by cmdlist-run, with specified
;;      position
;;    + Replace a command by cmdlist-replace, with specified position
;;      and new command
;;    + Remove a command by cmdlist-remove, with specified position
;;    + Clear whole cmdlist by cmdlist-clear

;;====================================================================
;; CODE:

(defun cmdlist-add(text)
  "Add a new command to cmdlist"
  (interactive "sCommand to be added: ")
  (setq tnt_cmdlist
	(append (if (boundp 'tnt_cmdlist)
		    tnt_cmdlist
		  ())
		(list text))
	)
  (message (concat "Command saved at position "
		   (number-to-string (length tnt_cmdlist))))
  )

(defun cmdlist-show()
  "Show current cmdlist"
  (interactive)
  (if (boundp 'tnt_cmdlist)
      (with-output-to-temp-buffer "*cmdlist/mptnt1988*"
	(princ (let ((i 0) (result ""))
		 (dolist (var tnt_cmdlist result)
		   (setq i (+ i 1))
		   (setq result
			 (concat result
				 (number-to-string i)
				 "\t" var "\n"))))))
    (message "There is no command in cmdlist"))
  )

(defun cmdlist-run(pos)
  "Run the command at position pos in cmdlist"
  (interactive "nRun command at position: ")
  (async-shell-command (nth (- pos 1) tnt_cmdlist))
  )

(defun cmdlist-remove(pos)
  "Remove the command at position pos in cmdlist"
  (interactive "nRemove command at position: ")
  (setq tnt_cmdlist (append
		     (butlast tnt_cmdlist
			      (- (length tnt_cmdlist) pos -1))
		     (nthcdr pos tnt_cmdlist)))
  (cmdlist-show)
  )


(defun cmdlist-replace(pos cmd)
  "Replace the command at position pos in cmdlist by new command cmd"
  (interactive "nReplace command at position: \ns...with command: ")
  (setcar (nthcdr (- pos 1) tnt_cmdlist) cmd)
  (cmdlist-show)
  )

(defun cmdlist-clear()
  "Clear whole cmdlist"
  (interactive)
  (makunbound 'tnt_cmdlist)
  )
