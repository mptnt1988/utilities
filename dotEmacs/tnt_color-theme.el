;; Note:
;;   Install color-theme and color-theme-solarized from Packages List
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_color-theme.el")

(setq color-theme-is-global t)
(require 'color-theme)
(color-theme-initialize)
(run-at-time "10 sec" nil 'color-theme-billw)
