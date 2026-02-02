;;; md.el --- Markdown utilities -*- lexical-binding: t -*-

;;; Commentary:
;; Custom functions for working with Markdown files.

;;; Code:

;;;###autoload
(defun md-insert-date-heading (level)
  "Insert a date heading with LEVEL hash marks.
Default is 3 (###). With prefix arg, use that many hash marks.
For example, C-u 2 inserts ## 2026-01-17."
  (interactive "P")
  (let* ((level (or level 3))
         (hashes (make-string level ?#))
        (date (format-time-string "%Y-%m-%d")))
    (insert (format "%s %s\n" hashes date))))

(provide 'md)
;;; md.el ends here
