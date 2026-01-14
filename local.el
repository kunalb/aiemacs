;;; local.el --- Host specific initialization  -*- lexical-binding: t; -*-

(use-package nordic-night-theme
  :ensure t
  :config
  (load-theme 'nordic-night))
(set-face-attribute 'default nil :font "Berkeley Mono-9")

;;; Customize path on app launch
;; https://www.emacswiki.org/emacs/ExecPath
(defun set-exec-path-from-shell-PATH ()
  "Set up Emacs' `exec-path' and PATH environment variable to match
that used by the user's shell.

This is particularly useful under Mac OS X and macOS, where GUI
apps are not started from a shell."
  (interactive)
  (let ((path-from-shell
   (concat "/home/knl/bin:"
     (replace-regexp-in-string
      "[ \t\n]*$" "" (shell-command-to-string
    "$SHELL --login -c 'echo $PATH'"
    )))))
    (setenv "PATH" path-from-shell)
    (setq exec-path (split-string path-from-shell path-separator))))
(set-exec-path-from-shell-PATH)

;; WSL2 clipboard integration for emacs -nw
(defun wsl--local-default-directory ()
  (if (file-remote-p default-directory)
      (file-name-as-directory
       (or (getenv "USERPROFILE")
           (getenv "HOME")
           user-emacs-directory))
    default-directory))

(defun wsl-copy (text &optional _push)
  (let ((default-directory (wsl--local-default-directory))
        (process-connection-type nil))
    (when (executable-find "clip.exe")
      (let ((proc (start-process "clip.exe" "*Messages*" "clip.exe")))
        (process-send-string proc text)
        (process-send-eof proc)))))

(defun wsl-paste ()
  (let ((default-directory (wsl--local-default-directory)))
    (when (executable-find "powershell.exe")
      (let ((raw (shell-command-to-string
                  "powershell.exe -NoProfile -Command \"[Console]::Out.Write((Get-Clipboard -Raw))\"")))
        (replace-regexp-in-string "\r" "" raw)))))

(when (string-match-p "microsoft"
                      (downcase (shell-command-to-string "uname -r")))
  (setq interprogram-cut-function #'wsl-copy)
  (setq interprogram-paste-function #'wsl-paste))


(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs
   `((python-ts-mode python-mode) . ("pyrefly" "lsp"))))


(use-package company
  :ensure t
  :hook (eglot-managed-mode . company-mode))

; (use-package yasnippet
;   :ensure t
;   :config
;   (yas-global-mode 1))
;
; (use-package yasnippet-snippets
;   :ensure t
;   :after yasnippet)

(use-package olivetti)


(use-package gptel
  :ensure t
  :config

  ;; DeepSeek offers an OpenAI compatible API
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key "sk-aaf43d13fe7a430abd2c2bf98ebba305"
    :models '(deepseek-chat deepseek-reasoner))

  (gptel-make-anthropic "Claude"
    :stream t
    :key "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")

  (setq gptel-backend
  (gptel-make-openai "Foundry"
    :host "172.29.80.1:1234"
    :endpoint "/v1/chat/completions"
    :protocol "http"
    :stream t
    :models '(qwen2.5-7b-instruct-qnn-npu:1 deepseek-r1-distill-qwen-14b-qnn-npu:1))))

(use-package plisp-mode
  :ensure t)

(use-package conda
  :ensure t)


; https://mclare.blog/posts/using-uv-in-emacs/
; Modified with Gemini to support Ctrl-U to manually choose an environment path
(defun uv-activate (&optional arg)
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter."
  (interactive "P")
  (let* ((project-root (when-let ((proj (project-current)))
                         (project-root proj)))
         (default-root (or project-root default-directory))
   (venv-path (if arg
                  (read-directory-name "Select venv directory: " default-root)
                (if project-root
                    (expand-file-name ".venv" project-root)
                  (error "Not in a project; use C-u to specify venv path manually"))))
   (python-exe (if (eq system-type 'windows-nt)
       "Scripts/python.exe"
     "bin/python"))
   (python-path (expand-file-name python-exe venv-path)))
    (if (file-exists-p python-path)
  (progn
    ;; Set Python interpreter path
    (setq-local python-shell-interpreter python-path)
    (setq-local python-shell-virtualenv-root venv-path)

    ;; Update exec-path to include the venv's bin directory
    (let ((venv-bin-dir (file-name-directory python-path)))
      (setq-local exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

    ;; Ensure env changes are buffer-local (important for TRAMP)
    (setq-local process-environment (copy-sequence process-environment))

    ;; Update PATH environment variable
    (setenv "PATH" (concat (file-name-directory python-path)
   path-separator
   (getenv "PATH")))

    ;; Update VIRTUAL_ENV environment variable
    (setenv "VIRTUAL_ENV" venv-path)

    ;; Remove PYTHONHOME if it exists
    (setenv "PYTHONHOME" nil)

    (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" venv-path))))

; (use-package copilot
;   :vc (:url "https://github.com/copilot-emacs/copilot.el"
;     :rev :newest
;     :branch "main"))

; ;; for eat terminal backend:
; (use-package eat
;   :straight (:type git
;      :host codeberg
;      :repo "akib/emacs-eat"
;      :files ("*.el" ("term" "term/*.el") "*.texi"
;        "*.ti" ("terminfo/e" "terminfo/e/*")
;        ("terminfo/65" "terminfo/65/*")
;        ("integration" "integration/*")
;        (:exclude ".dir-locals.el" "*-tests.el"))))

;; for vterm terminal backend:
(use-package vterm :straight t)

(use-package eca)

(use-package agent-shell
  :straight t
  :ensure t)


;; Prevent flickering at the edges
(setq display-line-numbers-width 4)

(straight-use-package 'poet-theme)
(straight-use-package 'ef-themes)
(straight-use-package 'acme-theme)
(straight-use-package 'blacken)
(straight-use-package 'magit)

(use-package eglot-python-preset
  :ensure t
  :after eglot
  :custom (eglot-python-preset-lsp-server 'ty)
  :config (eglot-python-preset-setup))

;; Insert a compact timestamp for markdown notes.
(defun knl-insert-timestamp ()
  "Insert the current date/time."
  (interactive)
  (insert (format-time-string "%Y-%m-%d %H:%M")))

(setq auto-save-visited-interval 2)

; remove the comments and just have the code
; (use-package
;   whisper.el
;   :custom (whisper-model "small"))

; (straight-use-package
;  '(monet :type git :host github :repo "stevemolitor/monet"))
;
; ;; install claude-code.el, using :depth 1 to reduce download size:
; (use-package claude-code
;   :straight (:type git :host github :repo "stevemolitor/claude-code.el" :branch "main" :depth 1
;      :files ("*.el" (:exclude "images/*")))
;   :bind-keymap
;   ("C-c c" . claude-code-command-map) ;; or your preferred key
;   ;; Optionally define a repeat map so that "M" will cycle thru Claude auto-accept/plan/confirm modes after invoking claude-code-cycle-mode / C-c M.
;   :bind
;   (:repeat-map my-claude-code-map ("M" . claude-code-cycle-mode))
;   :config
;   ;; optional IDE integration with Monet
;   (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
;   (monet-mode 1)
;   (claude-code-mode))
;
; ;;; AI Code interface
; (use-package ai-code-interface
;   :straight (:host github :repo "tninja/ai-code-interface.el")
;   :config
;   (ai-code-set-backend  'codex)
;   (global-set-key (kbd "C-c a") #'ai-code-menu)
;   (with-eval-after-load 'magit
;     (ai-code-magit-setup-transients)))
