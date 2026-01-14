;;; clipboard.el --- WSL2 clipboard integration  -*- lexical-binding: t; -*-

(defun wsl-copy (text &optional _push)
  "Copy TEXT to Windows clipboard via clip.exe."
  (let ((process-connection-type nil))
    (let ((proc (start-process "clip" nil "clip.exe")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun wsl-paste ()
  "Paste from Windows clipboard via PowerShell."
  (replace-regexp-in-string
   "\r" ""
   (shell-command-to-string
    "powershell.exe -NoProfile -Command \"[Console]::Out.Write((Get-Clipboard -Raw))\"")))

(defun clipboard-setup ()
  "Set up WSL clipboard integration if running under WSL."
  (when (and (eq system-type 'gnu/linux)
             (string-match-p "microsoft"
                             (downcase (shell-command-to-string "uname -r"))))
    (setq interprogram-cut-function #'wsl-copy)
    (setq interprogram-paste-function #'wsl-paste)))

(provide 'clipboard)
;;; clipboard.el ends here
