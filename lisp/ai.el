;;; ai.el --- AI agent integration via agent-shell  -*- lexical-binding: t; -*-

;;; Commentary:
;; Invoke AI agents with context from current buffer.
;; Uses agent-shell for agent communication and interactive diff handling.

(require 'agent-shell)

(defvar ai-default-backend 'claude
  "Default AI backend. One of: claude, gemini, codex, opencode.")

(defvar ai-last-instruction nil
  "Last instruction sent to an agent.")

(defun ai--get-config (backend)
  "Get agent-shell config for BACKEND."
  (pcase backend
    ('claude (agent-shell-anthropic-make-claude-code-config))
    ('gemini (agent-shell-google-make-gemini-cli-config))
    ('codex (agent-shell-openai-make-codex-config))
    ('opencode (agent-shell-opencode-make-agent-config))
    (_ (error "Unknown backend: %s" backend))))

(defun ai--gather-context ()
  "Gather context from current buffer state.
Returns plist with :file, :line, :region (if active)."
  (let ((context (list :file (buffer-file-name)
                       :line (line-number-at-pos))))
    (when (use-region-p)
      (plist-put context :region
                 (buffer-substring-no-properties
                  (region-beginning) (region-end)))
      (plist-put context :region-start (line-number-at-pos (region-beginning)))
      (plist-put context :region-end (line-number-at-pos (region-end))))
    context))

(defun ai--format-prompt (instruction context)
  "Format INSTRUCTION with CONTEXT for the agent."
  (let ((file (plist-get context :file))
        (region (plist-get context :region))
        (line (plist-get context :line)))
    (concat
     (when file
       (format "File: %s\n" file))
     (if region
         (format "Lines %d-%d:\n```\n%s\n```\n\n"
                 (plist-get context :region-start)
                 (plist-get context :region-end)
                 region)
       (format "Cursor at line %d\n\n" line))
     instruction)))

(defun ai--ensure-shell (backend)
  "Ensure an agent-shell exists for BACKEND, return buffer."
  (let* ((config (ai--get-config backend))
         (buffer-name (map-elt config :buffer-name))
         (existing (seq-find
                    (lambda (buf)
                      (with-current-buffer buf
                        (and (derived-mode-p 'agent-shell-mode)
                             (string-match-p (regexp-quote buffer-name)
                                             (buffer-name)))))
                    (buffer-list))))
    (or existing
        (progn
          (agent-shell-start :config config)
          (get-buffer (format "*%s*" buffer-name))))))

(defun ai--read-instruction ()
  "Read instruction from user, with last instruction as default."
  (let ((prompt (if ai-last-instruction
                    (format "Instruction [%s]: "
                            (truncate-string-to-width ai-last-instruction 30 nil nil "..."))
                  "Instruction: ")))
    (let ((input (read-string prompt nil nil ai-last-instruction)))
      (when (string-empty-p input)
        (user-error "No instruction provided"))
      (setq ai-last-instruction input)
      input)))

;;;###autoload
(defun ai-prompt (&optional backend)
  "Prompt for instruction and send to agent with current context.
BACKEND is one of: claude, gemini, codex, opencode."
  (interactive)
  (let* ((backend (or backend ai-default-backend))
         (context (ai--gather-context))
         (instruction (ai--read-instruction))
         (prompt (ai--format-prompt instruction context)))
    (when (plist-get context :file)
      (save-buffer))
    (ai--ensure-shell backend)
    (agent-shell-insert :text prompt :submit t)))

;;;###autoload
(defun ai-prompt-region (start end &optional backend)
  "Prompt for instruction with region as context.
START and END define the region. BACKEND specifies which agent to use."
  (interactive "r")
  (let* ((backend (or backend ai-default-backend))
         (context (list :file (buffer-file-name)
                        :line (line-number-at-pos start)
                        :region (buffer-substring-no-properties start end)
                        :region-start (line-number-at-pos start)
                        :region-end (line-number-at-pos end)))
         (instruction (ai--read-instruction))
         (prompt (ai--format-prompt instruction context)))
    (when (buffer-file-name)
      (save-buffer))
    (ai--ensure-shell backend)
    (agent-shell-insert :text prompt :submit t)))

;;;###autoload
(defun ai-prompt-buffer (&optional backend)
  "Open a buffer to compose a longer instruction."
  (interactive)
  (let* ((backend (or backend ai-default-backend))
         (context (ai--gather-context))
         (source-buffer (current-buffer))
         (buf (get-buffer-create "*ai-instruction*")))
    (when (plist-get context :file)
      (with-current-buffer source-buffer
        (save-buffer)))
    (pop-to-buffer buf)
    (erase-buffer)
    (ai-instruction-mode)
    (setq-local ai--context context)
    (setq-local ai--backend backend)
    (setq-local ai--source-buffer source-buffer)
    (insert "# Type instruction below, then C-c C-c to send, C-c C-k to cancel\n")
    (insert "# Context: " (or (plist-get context :file) "no file"))
    (when (plist-get context :region)
      (insert (format " (lines %d-%d selected)"
                      (plist-get context :region-start)
                      (plist-get context :region-end))))
    (insert "\n\n")))

(defvar-local ai--context nil "Context for current instruction buffer.")
(defvar-local ai--backend nil "Backend for current instruction buffer.")
(defvar-local ai--source-buffer nil "Source buffer for current instruction.")

(defun ai-instruction-send ()
  "Send the instruction from the instruction buffer."
  (interactive)
  (let* ((content (buffer-string))
         ;; Strip header comments
         (instruction (replace-regexp-in-string
                       "^#.*\n" ""
                       content))
         (instruction (string-trim instruction))
         (context ai--context)
         (backend ai--backend)
         (prompt (ai--format-prompt instruction context)))
    (when (string-empty-p instruction)
      (user-error "No instruction provided"))
    (setq ai-last-instruction instruction)
    (quit-window t)
    (ai--ensure-shell backend)
    (agent-shell-insert :text prompt :submit t)))

(defun ai-instruction-cancel ()
  "Cancel the instruction buffer."
  (interactive)
  (quit-window t))

(define-derived-mode ai-instruction-mode text-mode "AI-Inst"
  "Mode for composing AI instructions."
  (setq-local header-line-format
              '(" C-c C-c: send | C-c C-k: cancel")))

(define-key ai-instruction-mode-map (kbd "C-c C-c") #'ai-instruction-send)
(define-key ai-instruction-mode-map (kbd "C-c C-k") #'ai-instruction-cancel)

;; Backend-specific commands
;;;###autoload
(defun ai-claude ()
  "Prompt for instruction and send to Claude."
  (interactive)
  (ai-prompt 'claude))

;;;###autoload
(defun ai-gemini ()
  "Prompt for instruction and send to Gemini."
  (interactive)
  (ai-prompt 'gemini))

;;;###autoload
(defun ai-codex ()
  "Prompt for instruction and send to Codex."
  (interactive)
  (ai-prompt 'codex))

;;;###autoload
(defun ai-repeat ()
  "Repeat last instruction with current context."
  (interactive)
  (unless ai-last-instruction
    (user-error "No previous instruction"))
  (let* ((context (ai--gather-context))
         (prompt (ai--format-prompt ai-last-instruction context)))
    (when (plist-get context :file)
      (save-buffer))
    (ai--ensure-shell ai-default-backend)
    (agent-shell-insert :text prompt :submit t)))

;;;###autoload
(defun ai-setup-keys ()
  "Set up AI keybindings under C-c a prefix."
  ;; Main entry points
  (global-set-key (kbd "C-c a a") #'ai-prompt)        ; prompt with context
  (global-set-key (kbd "C-c a b") #'ai-prompt-buffer) ; multi-line instruction
  (global-set-key (kbd "C-c a r") #'ai-prompt-region) ; explicit region
  (global-set-key (kbd "C-c a .") #'ai-repeat)        ; repeat last
  ;; Backend shortcuts
  (global-set-key (kbd "C-c a c") #'ai-claude)
  (global-set-key (kbd "C-c a g") #'ai-gemini)
  (global-set-key (kbd "C-c a x") #'ai-codex))

(provide 'ai)
;;; ai.el ends here
