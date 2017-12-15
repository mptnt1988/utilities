;;; tnt_web-mode --- Module for web-mode
;;;---------------------------------------------------------
;;; Commentary:
;;  Usage:
;;    (require 'tnt_web-mode)

;;;---------------------------------------------------------
;;; Code:

(require 'tntLib)

(setq tntLib-myPkgs
  '(
    web-mode
    php-mode
    typescript-mode
    ))

(defun tnt_web-mode_installation-hook ()
  "Auto install tslint and typescript."
  (message "mptnt1988: Auto install tnt_web-mode...
typescript tslint & typescript")
  (let
      ((tntinstall "npm install -g tslint typescript"))
    (call-process-shell-command tntinstall)))
(tntLib_install-myPkgs 'tnt_web-mode_installation-hook)

;;------------------
;; WEB MODE
;;------------------

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.blade\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("/\\(views\\|html\\|templates\\)/.*\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"    . "\\.phtml\\'")
        ("blade"  . "\\.blade\\.")))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (local-set-key (kbd "RET") 'newline-and-indent))
(add-hook 'web-mode-hook 'my-web-mode-hook)

;; If smartparens installed, add more pair for web-mode
(if (package-installed-p 'smartparens)
    (progn
      (require 'smartparens)
      (defun sp-web-mode-is-code-context (id action context)
        (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side)))))
      (sp-local-pair 'web-mode "<" nil :when '(sp-web-mode-is-code-context))))

;;------------------
;; PHP MODE
;;------------------

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . php-mode))

;;------------------
;; TYPESCRIPT MODE
;;------------------

(require 'typescript-mode)

;; *mptnt1988*:
;;   sp-local-pair is defined by cl-defun
;;   Currently, not able to remove cl related warning
(provide 'tnt_web-mode)
;;; tnt_web-mode.el ends here
