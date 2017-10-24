;; TODO:
;; - Highlight region and add to list
;; - Open buffer to write note
;; - Save buffer to file
;; - Load buffer from file
;; - Display note at point
;; - Open buffer to display note

(require 'highlight)

(defvar-local mpr-var-note-list '() "List of review note.")

(defun mpr-insert ()
  (interactive)
  (message "mpr test insert"))

(defface mpreview-face
  '((t :inherit highlight
       :weight bold))
  "Face for displaying reviewed text."
  :group 'convenience)

(defun mpr_set-color (bg-color fg-color)
  (set-face-attribute 'mpreview-face nil
                      :foreground fg-color
                      :background bg-color))

;;;###autoload
(define-minor-mode mpreview-mode
  "Toggle review mode."
  :lighter " mp-review"
  ;; :keymap (let ((map (make-sparse-keymap)))
  ;;           (define-key map (kbd "C-c f") 'mpr-insert)
  ;;           map)
  )

(provide 'mp-review)
