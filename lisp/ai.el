;;; ai.el --- AI agent integration  -*- lexical-binding: t; -*-

;;; Commentary:
;; Execute AI agents on file with instructions from comments/region.
;; Supports claude, codex, and gemini CLIs.

(defvar ai-default-backend 'claude
  "Default AI backend. One of: claude, codex, gemini.")

(defvar ai-backends
  '((claude . "claude")
    (codex . "codex")
    (gemini . "gemini"))
  "Alist mapping backend symbols to CLI commands.")

(defun ai--get-instruction (start end)
  "Extract instruction from region, stripping comment markers."
  (let ((text (buffer-substring-no-properties start end)))
    (string-trim
     (replace-regexp-in-string
      "^[ \t]*\\(?://\\|#\\|;+\\|\\*\\|--\\)[ \t]*" ""
      text))))

(defun ai--build-prompt (instruction file line)
  "Build prompt for AI agent."
  (format "In file %s around line %d: %s" file line instruction))

(defun ai-execute-region (start end &optional backend)
  "Execute AI agent on current file with instruction from region.
BACKEND is one of: claude, codex, gemini. Prompts if not specified."
  (interactive "r")
  (let* ((backend (or backend
                      (intern (completing-read "Backend: " '("claude" "codex" "gemini")
                                               nil t nil nil
                                               (symbol-name ai-default-backend)))))
         (cli (alist-get backend ai-backends))
         (file (buffer-file-name))
         (line (line-number-at-pos start))
         (instruction (ai--get-instruction start end))
         (prompt (ai--build-prompt instruction file line))
         (default-directory (file-name-directory file)))
    (unless cli
      (error "Unknown backend: %s" backend))
    (unless file
      (error "Buffer must be visiting a file"))
    ;; Save buffer before letting agent work on it
    (save-buffer)
    ;; Launch agent in vterm
    (ai--launch-agent cli prompt default-directory)))

(defun ai--launch-agent (cli prompt dir)
  "Launch CLI agent in vterm with PROMPT."
  (let ((vterm-shell (format "%s \"%s\"" cli (shell-quote-argument prompt)))
        (default-directory dir))
    (vterm (format "*ai:%s*" cli))))

(defun ai-execute-with-claude (start end)
  "Execute claude on region instruction."
  (interactive "r")
  (ai-execute-region start end 'claude))

(defun ai-execute-with-codex (start end)
  "Execute codex on region instruction."
  (interactive "r")
  (ai-execute-region start end 'codex))

(defun ai-execute-with-gemini (start end)
  "Execute gemini on region instruction."
  (interactive "r")
  (ai-execute-region start end 'gemini))

(defun ai-execute-line (&optional backend)
  "Execute AI agent with instruction from current line."
  (interactive)
  (ai-execute-region (line-beginning-position) (line-end-position) backend))

(defun ai-setup-keys ()
  "Set up AI keybindings under C-c a prefix."
  (global-set-key (kbd "C-c a a") 'ai-execute-region)
  (global-set-key (kbd "C-c a l") 'ai-execute-line)
  (global-set-key (kbd "C-c a c") 'ai-execute-with-claude)
  (global-set-key (kbd "C-c a x") 'ai-execute-with-codex)
  (global-set-key (kbd "C-c a g") 'ai-execute-with-gemini))

(provide 'ai)
;;; ai.el ends here
