;;; clipboard.el --- WSL2 clipboard integration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Explicit Windows clipboard commands. Emacs kill-ring stays fast by default.
;; Use these commands only when you need to interact with Windows apps.

;;;###autoload
(defun wsl-copy-region (start end)
  "Copy region to Windows clipboard."
  (interactive "r")
  (let ((text (buffer-substring-no-properties start end))
        (process-connection-type nil))
    (let ((proc (start-process "clip" nil "clip.exe")))
      (process-send-string proc text)
      (process-send-eof proc))
    (message "Copied to Windows clipboard")))

;;;###autoload
(defun wsl-copy-kill ()
  "Copy current kill to Windows clipboard."
  (interactive)
  (let ((text (current-kill 0))
        (process-connection-type nil))
    (let ((proc (start-process "clip" nil "clip.exe")))
      (process-send-string proc text)
      (process-send-eof proc))
    (message "Copied kill to Windows clipboard")))

;;;###autoload
(defun wsl-paste ()
  "Paste from Windows clipboard at point."
  (interactive)
  (insert
   (replace-regexp-in-string
    "\r" ""
    (shell-command-to-string
     "powershell.exe -NoProfile -Command \"[Console]::Out.Write((Get-Clipboard -Raw))\""))))

(provide 'clipboard)
;;; clipboard.el ends here
