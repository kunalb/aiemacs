;;; ai.el --- AI agent integration via agent-shell  -*- lexical-binding: t; -*-

;;; Commentary:
;; Execute AI agents on file with instructions from comments/region.
;; Uses agent-shell for agent communication and interactive diff handling.

(require 'agent-shell)

(defvar ai-default-backend 'claude
  "Default AI backend. One of: claude, gemini, codex, opencode.")

(defun ai--get-config (backend)
  "Get agent-shell config for BACKEND."
  (pcase backend
    ('claude (agent-shell-anthropic-make-claude-code-config))
    ('gemini (agent-shell-google-make-gemini-cli-config))
    ('codex (agent-shell-openai-make-codex-config))
    ('opencode (agent-shell-opencode-make-agent-config))
    (_ (error "Unknown backend: %s" backend))))

(defun ai--strip-comment-markers (text)
  "Strip common comment markers from TEXT."
  (string-trim
   (replace-regexp-in-string
    "^[ \t]*\\(?://\\|#\\|;+\\|\\*\\|--\\)[ \t]*" ""
    text)))

(defun ai--build-prompt (instruction file line)
  "Build prompt for AI agent with INSTRUCTION, FILE and LINE context."
  (format "In file `%s` around line %d:\n\n%s"
          (file-name-nondirectory file) line instruction))

(defun ai--ensure-shell (backend)
  "Ensure an agent-shell exists for BACKEND, creating one if needed.
Returns the shell buffer."
  (let* ((config (ai--get-config backend))
         (buffer-name (map-elt config :buffer-name))
         (existing (seq-find
                    (lambda (buf)
                      (with-current-buffer buf
                        (and (derived-mode-p 'agent-shell-mode)
                             (string= (buffer-name) (format "*%s*" buffer-name)))))
                    (buffer-list))))
    (if existing
        existing
      ;; Start new shell
      (agent-shell-start :config config)
      (get-buffer (format "*%s*" buffer-name)))))

(defun ai-execute-region (start end &optional backend)
  "Execute AI agent on current file with instruction from region.
BACKEND is one of: claude, gemini, codex, opencode. Prompts if not specified."
  (interactive "r")
  (let* ((backend (or backend
                      (intern (completing-read
                               "Backend: "
                               '("claude" "gemini" "codex" "opencode")
                               nil t nil nil
                               (symbol-name ai-default-backend)))))
         (file (buffer-file-name))
         (line (line-number-at-pos start))
         (instruction (ai--strip-comment-markers
                       (buffer-substring-no-properties start end)))
         (prompt (ai--build-prompt instruction file line)))
    (unless file
      (error "Buffer must be visiting a file"))
    ;; Save buffer before letting agent work on it
    (save-buffer)
    ;; Ensure shell exists and send prompt
    (ai--ensure-shell backend)
    (agent-shell-insert :text prompt :submit t)))

(defun ai-execute-with-claude (start end)
  "Execute Claude on region instruction."
  (interactive "r")
  (ai-execute-region start end 'claude))

(defun ai-execute-with-gemini (start end)
  "Execute Gemini on region instruction."
  (interactive "r")
  (ai-execute-region start end 'gemini))

(defun ai-execute-with-codex (start end)
  "Execute Codex on region instruction."
  (interactive "r")
  (ai-execute-region start end 'codex))

(defun ai-execute-line (&optional backend)
  "Execute AI agent with instruction from current line."
  (interactive)
  (ai-execute-region (line-beginning-position) (line-end-position) backend))

(defun ai-execute-line-claude ()
  "Execute Claude on current line instruction."
  (interactive)
  (ai-execute-line 'claude))

(defun ai-setup-keys ()
  "Set up AI keybindings under C-c a prefix."
  (global-set-key (kbd "C-c a a") 'ai-execute-region)
  (global-set-key (kbd "C-c a l") 'ai-execute-line)
  (global-set-key (kbd "C-c a c") 'ai-execute-with-claude)
  (global-set-key (kbd "C-c a g") 'ai-execute-with-gemini)
  (global-set-key (kbd "C-c a x") 'ai-execute-with-codex)
  ;; Quick access
  (global-set-key (kbd "C-c a RET") 'ai-execute-line-claude))

(provide 'ai)
;;; ai.el ends here
