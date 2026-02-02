;;; utils.el --- Utility functions  -*- lexical-binding: t; -*-

;;;###autoload
(defun insert-timestamp ()
  "Insert current date/time in compact format."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

;;;###autoload
(defun set-exec-path-from-shell ()
  "Sync exec-path with shell PATH."
  (interactive)
  (let ((path (replace-regexp-in-string
               "[ \t\n]*$" ""
               (shell-command-to-string "$SHELL --login -c 'echo $PATH'"))))
    (setenv "PATH" path)
    (setq exec-path (split-string path path-separator))))

(provide 'utils)
;;; utils.el ends here
