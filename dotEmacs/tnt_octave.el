;;; tnt_octave --- Setup and configure Emacs Octave support
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_octave)

;;;---------------------------------------------------------
;;; Code:

(setq auto-mode-alist
      (cons '("\\.m$" . octave-mode) auto-mode-alist))

(add-hook 'octave-mode-hook
          (lambda ()
            (abbrev-mode 1)
            (auto-fill-mode 1)
            (if (eq window-system 'x)
                (font-lock-mode 1))))

(provide 'tnt_octave)
;;; tnt_octave.el ends here
