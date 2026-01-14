;;; clipboard.el --- WSL2 clipboard integration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Fast clipboard integration for WSL2 using win32yank.
;; Install: curl -sLo /tmp/win32yank.zip https://github.com/equalsraf/win32yank/releases/download/v0.1.1/win32yank-x64.zip
;;          unzip -p /tmp/win32yank.zip win32yank.exe > ~/.local/bin/win32yank.exe
;;          chmod +x ~/.local/bin/win32yank.exe
;; Note: x64 binary runs through emulation on ARM Windows.

(defun wsl-copy (text &optional _push)
  "Copy TEXT to Windows clipboard."
  (let ((process-connection-type nil))
    (if (executable-find "win32yank.exe")
        (let ((proc (start-process "win32yank" nil "win32yank.exe" "-i" "--crlf")))
          (process-send-string proc text)
          (process-send-eof proc))
      ;; Fallback to clip.exe
      (when (executable-find "clip.exe")
        (let ((proc (start-process "clip" nil "clip.exe")))
          (process-send-string proc text)
          (process-send-eof proc))))))

(defun wsl-paste ()
  "Paste from Windows clipboard."
  (if (executable-find "win32yank.exe")
      (shell-command-to-string "win32yank.exe -o --lf")
    ;; Fallback to PowerShell (slow)
    (when (executable-find "powershell.exe")
      (replace-regexp-in-string
       "\r" ""
       (shell-command-to-string
        "powershell.exe -NoProfile -Command \"[Console]::Out.Write((Get-Clipboard -Raw))\"")))))

(defun clipboard-setup ()
  "Set up WSL clipboard integration if running under WSL."
  (when (and (eq system-type 'gnu/linux)
             (string-match-p "microsoft"
                             (downcase (shell-command-to-string "uname -r"))))
    (setq interprogram-cut-function #'wsl-copy)
    (setq interprogram-paste-function #'wsl-paste)))

(provide 'clipboard)
;;; clipboard.el ends here
