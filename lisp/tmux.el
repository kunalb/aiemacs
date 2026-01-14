;;; tmux.el --- Tmux integration  -*- lexical-binding: t; -*-

(defvar tmux--last-command nil
  "Last command sent to tmux.")

(defun tmux-send (command)
  "Send COMMAND to the currently active tmux pane."
  (interactive "sCommand: ")
  (setq tmux--last-command command)
  (call-process "tmux" nil nil nil "send-keys" command "Enter"))

(defun tmux-resend ()
  "Re-send the previous tmux command."
  (interactive)
  (if tmux--last-command
      (call-process "tmux" nil nil nil "send-keys" tmux--last-command "Enter")
    (message "No previous command")))

(defun tmux-send-buffer ()
  "Send buffer contents to tmux."
  (interactive)
  (call-process "tmux" nil nil nil "send-keys" (buffer-string) "Enter"))

(defun tmux-send-line ()
  "Send current line to tmux."
  (interactive)
  (call-process "tmux" nil nil nil "send-keys" (thing-at-point 'line) "Enter"))

(defun tmux-send-region (start end)
  "Send region to tmux."
  (interactive "r")
  (call-process "tmux" nil nil nil "send-keys" (buffer-substring start end) "Enter"))

(defun tmux-setup-keys ()
  "Set up tmux keybindings."
  (global-set-key (kbd "C-c x") 'tmux-send)
  (global-set-key (kbd "C-c r") 'tmux-resend)
  (global-set-key (kbd "C-c b") 'tmux-send-buffer)
  (global-set-key (kbd "C-c h") 'tmux-send-region)
  (global-set-key (kbd "C-c l") 'tmux-send-line))

(provide 'tmux)
;;; tmux.el ends here
