;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PATH TO LOAD EMACS CHUNK FILES     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(add-to-list 'load-path "/mnt/Source/dotEmacs/")
(load "tnt_package.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ERLANG MODE                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;--------------------------------------
;; [EDTS]
(load "tnt_edts.el")

;;--------------------------------------
;; [Flymake Erlang]
;; Below command is commented to avoid confusing with elpa's flymake.el
;; (add-to-list 'load-path "/mnt/Programs/emacs-flymake/")
(setq path-to-erl-flymake-escript
      "/mnt/Source/Erlang/escripts/flymake/flymake_erlang.es")
(load "tnt_flymake-erlang.el")

;;--------------------------------------
;; [Normal Erlang]
(load "tnt_erlang-mode.el")

;;--------------------------------------
;; [Distel]
(add-to-list 'load-path "/mnt/Programs/distel/elisp/")
(load "tnt_distel.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PYTHON MODE                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(load "tnt_python.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; COMMON SETTINGS                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
