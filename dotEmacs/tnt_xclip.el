;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_xclip.el")
(require 'xclip)

(global-set-key (kbd "C-M-y") 'x-clipboard-yank)
