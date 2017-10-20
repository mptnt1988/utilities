;; Example: colorname2hex("mediumspringgreen")

(defun colorname2hex(name)
  (concat "#"
	  (let (res)
	    (dolist (var (color-values name) res)
	      (setq res (concat res (to_hex var)))))))

(defun to_hex ( Num )
  (format "%04X" Num))
