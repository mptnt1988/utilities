;; Introduction:
;;   This module is used for setup and configure packages to manage projects
;; Note:
;;   Add path to this file to 'load-path
;;     (add-to-list 'load-path "/path/to/this/file")
;;   then
;;     (load "tnt_projects.el")

(defvar myPkgs_projects
  '(
    neotree
    projectile
    ack
    undo-tree
    ))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPkgs_projects)

(require 'projectile)
(projectile-global-mode)

(require 'neotree)
(defun neotree-projectile ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))

(require 'undo-tree)
(global-undo-tree-mode)
