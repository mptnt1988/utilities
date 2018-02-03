;;; tnt_elixir --- Module for Elixir
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_elixir)
;;  To go to Elixir built-in functions' definition:
;;    (setq alchemist-goto-elixir-source-dir "/path/to/elixir/")

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

(setq tntLib-myPkgs
      '(
        elixir-mode
        alchemist
        ))

(tntLib_install-myPkgs)

;; Enable alchemist
(require 'elixir-mode)
(require 'alchemist)
(add-hook 'elixir-mode-hook 'alchemist-mode)

;; Config company
(require 'company)

(global-company-mode 1)

(setq company-idle-delay 0.1)
(setq company-tooltip-limit 10)
(setq company-minimum-prefix-length 2)
(setq company-tooltip-flip-when-above t)

(provide 'tnt_elixir)
;;; tnt_elixir.el ends here
