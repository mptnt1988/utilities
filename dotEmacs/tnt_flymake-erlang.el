;; Usage:
;;   Use command flymake-mode to toggle the buffer's mode
;; Notes:
;;   0.Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;     then
;;     (load "tnt_flymake-erlang.el")
;;   1.Add path to flymake.el to 'load-path
;;     (add-to-list 'load-path "/path/to/flymake.el")
;;   2.Set path-to-erl-flymake-escript global variable:
;;     (setq path-to-erl-flymake-escript "/path/to/escript")
(require 'flymake)
(defun flymake-erlang-init ()
  (let* ((temp-file (flymake-init-create-temp-buffer-copy
		     'flymake-create-temp-inplace))
	 (local-file (file-relative-name temp-file
					 (file-name-directory buffer-file-name))))
    (list path-to-erl-flymake-escript (list local-file))))

(add-to-list 'flymake-allowed-file-name-masks '("\\.erl\\'" flymake-erlang-init))
