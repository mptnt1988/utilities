;;; --- bookmark-line.el ---
;;; Line bookmarks for Emacs buffer

;;====================================================================
;; Author: Tuan N. Tran <mptnt1988@gmail.com>
;; Created: 20 Oct 2017
;; Version: 1.0

;;====================================================================
;; REVISION:
;; 1.0         Firstly created

;;====================================================================
;; GUIDE:
;; - Public APIs:
;;    + bl_add-line
;;    + bl_remove-line
;;    + bl_remove-all
;;    + bl_next-bm
;;    + bl_previous-bm
;;    + bl_set-color
;;    + Variable mp-bookmark-char
;; - Usages:
;;    + Use bl_add-line to add new bookmark at current line
;;    + Use bl_remove-line to remove the bookmark at current line,
;;      and bl_remove-all to remove all bookmarks
;;    + Use bl_next-bm and bl_previous-bm to move among bookmarks
;;    + Default bookmark char ★ can be configured by setting variable
;;      mp-bookmark-char
;;    + Default bookmark color "color-226" can be configured by
;;      bl_set-color

;;====================================================================
;; CODE:

(require 'linum)

(defvar-local mp-bookmark-list '() "List of bookmarked linums.")
(defvar mp-bookmark-char "★" "Character used as bookmark symbol.")
(defvar mp-border-width 1 "Border width for linum.")

(defface linum-bookmark-face
  '((t :inherit linum
       :foreground "color-226"))
  "Face for displaying the bookmarked line."
  :group 'linum)

(defun bl_set-color (color)
  (set-face-attribute 'linum-bookmark-face nil
                      :foreground color))

(defadvice linum-update (before advice-linum-update activate)
  "Set border width for the line number space."
  (setq mp-border-width
        (number-to-string
         (length (number-to-string (count-lines (point-min)
                                                (point-max)))))))

(defun bl_check-mark-current-line (line-number)
  "Highlight the current line number using `linum-bookmark-face'."
  (let* ((the-line-p (member line-number mp-bookmark-list))
         (chosen-face (if the-line-p 'linum-bookmark-face
                        'linum)))
    (propertize (format (concat "%" mp-border-width
                                (if the-line-p
                                    (concat "d" mp-bookmark-char)
                                  "d"))
                        line-number)
                    'face chosen-face)))

(setq linum-format 'bl_check-mark-current-line)

(defun bl_add-line ()
  "Add the current line number to list."
  (interactive)
  (let* ((current-linum (line-number-at-pos))
         (existed-p (member current-linum mp-bookmark-list)))
    (unless existed-p
      (let ((new-bm-list (add-to-list 'mp-bookmark-list
                                       current-linum)))
            (setq mp-bookmark-list (sort new-bm-list '<))))))

(defun bl_remove-line ()
  "Remove the current line number from list."
  (interactive)
  (setq mp-bookmark-list
        (remove (line-number-at-pos) mp-bookmark-list)))

(defun bl_remove-all ()
  "Remove all from list."
  (interactive)
  (setq mp-bookmark-list '()))

(defun bl_next-bm ()
  "Jump to next bookmarked line."
  (interactive)
  (let ((current-linum (line-number-at-pos))
        (len (length mp-bookmark-list))
        (i 0))
    (while (and (< i len)
                (>= current-linum (nth i mp-bookmark-list)))
      (setq i (1+ i)))
    (if (= i len)
        (message "No next bookmark.")
      (goto-line (nth i mp-bookmark-list)))))

(defun bl_previous-bm ()
  "Jump to previous bookmarked line."
  (interactive)
  (let ((current-linum (line-number-at-pos))
        (len (length mp-bookmark-list))
        (i 0))
    (while (and (< i len)
                (> current-linum (nth i mp-bookmark-list)))
      (setq i (1+ i)))
    (if (= i 0)
        (message "No previous bookmark.")
      (goto-line (nth (- i 1) mp-bookmark-list)))))

(provide 'bookmark-line)
