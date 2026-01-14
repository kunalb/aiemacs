;;; init.el --- Emacs initialization file  -*- lexical-binding: t; -*-

;;; Commentary:
;; A refreshed emacs configuration with my most used packages and configurations.

;;;; ============================================================================
;;;; PACKAGE MANAGEMENT
;;;; ============================================================================

(setq package-enable-at-startup nil)

;;; Bootstrap straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; Bootstrap use-package
(setq straight-use-package-by-default t)
(straight-use-package 'use-package)
(straight-use-package 'bind-key)
(require 'use-package)
(require 'bind-key)

(use-package project
  :straight (:type built-in))

;;;; ============================================================================
;;;; CORE SETUP
;;;; ============================================================================

;;; Load custom lisp
(add-to-list 'load-path (concat user-emacs-directory "lisp"))

;;; Customization file
(setq custom-file (concat user-emacs-directory "custom.el"))
(load custom-file 'noerror)

;;; Path configuration
(require 'utils)
(set-exec-path-from-shell)

;;; Clipboard
(require 'clipboard)

;;;; ============================================================================
;;;; UI CONFIGURATION
;;;; ============================================================================

;;; Disable unnecessary UI elements
(tool-bar-mode -1)
(menu-bar-mode -1)
(when (boundp 'fringe-mode) (fringe-mode -1))
(when (boundp 'scroll-bar-mode) (scroll-bar-mode -1))

;;; Margins
(setq-default left-margin-width 1 right-margin-width 1)

;;; Mouse settings
(xterm-mouse-mode)
(setq mouse-wheel-progressive-speed nil
      focus-follows-mouse "auto-raise"
      mouse-autoselect-window t)

;;; Theme management
(defun clear-previous-themes (&rest _)
  "Clear existing theme settings instead of layering them."
  (mapc #'disable-theme custom-enabled-themes))
(advice-add 'load-theme :before #'clear-previous-themes)

;;; Startup settings
(setq inhibit-startup-message t
      inhibit-splash-screen t
      initial-scratch-message nil
      ring-bell-function 'ignore)

;;; Window management
(winner-mode t)

;;; Display settings
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(pixel-scroll-mode)
(setq compilation-window-height 15)
(setq-default truncate-lines t)
(setq display-line-numbers-width 4)

;;; Mode line
(setq mode-line-format
      (list "%& %b%n" " ~ " "%m" " ~ " "%l:%c"))

;;; Performance
(setq-default xterm-query-timeout nil)

;;;; ============================================================================
;;;; EVIL MODE
;;;; ============================================================================

(use-package evil
  :init
  (setq evil-respect-visual-line-mode t
        evil-want-keybinding nil
        evil-want-integration t)
  :config
  (evil-mode t)
  (evil-ex-define-cmd "W[rite]" 'save-buffer)
  (evil-ex-define-cmd "V[split]" 'evil-window-vsplit))

(use-package evil-collection
  :config
  (evil-collection-init))

;;;; ============================================================================
;;;; EDITING
;;;; ============================================================================

;;; Indentation and formatting
(setq-default c-basic-offset 2
              tab-width 2
              indent-tabs-mode nil
              auto-save-default nil
              backup-directory-alist `((".*" . ,temporary-file-directory))
              auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
(setq js-indent-level 2)
(setq auto-save-visited-interval 2)

;;; Smartparens
(use-package smartparens
  :config
  (require 'smartparens-config)
  (add-hook 'prog-mode-hook 'turn-on-smartparens-mode)
  (define-key smartparens-mode-map (kbd "M-f") 'sp-forward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-b") 'sp-backward-slurp-sexp)
  (define-key smartparens-mode-map (kbd "M-F") 'sp-forward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-B") 'sp-backward-barf-sexp)
  (define-key smartparens-mode-map (kbd "M-s") 'sp-splice-sexp)
  (define-key smartparens-mode-map (kbd "C-k") 'sp-kill-sexp))

;;; Parenthesis highlighting
(show-paren-mode t)

;;; Whitespace
(add-hook 'before-save-hook 'whitespace-cleanup)
(setq require-final-newline t)

;;;; ============================================================================
;;;; COMPLETION
;;;; ============================================================================

(use-package ivy
  :config (ivy-mode 1))

(use-package counsel
  :config (counsel-mode 1))

(use-package company
  :hook (prog-mode . company-mode)
  :config
  (add-hook 'company-mode-hook
            (lambda ()
              (define-key evil-insert-state-map (kbd "C-.") 'company-complete)))
  (setq company-tooltip-align-annotations t
        company-idle-delay 0.1
        company-minimum-prefix-length 2))

;;;; ============================================================================
;;;; ORG MODE
;;;; ============================================================================

(use-package org
  :config
  (setq org-src-window-setup 'other-window
        org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-edit-src-content-indentation 0
        org-fontify-quote-and-verse-blocks t
        org-confirm-babel-evaluate nil
        org-hide-emphasis-markers t
        org-startup-with-inline-images t
        org-fast-tag-selection-single-key 'expert)

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t) (dot . t) (ditaa . t) (python . t) (C . t) (shell . t)))

  (add-to-list 'org-src-lang-modes '("html" . web))
  (add-hook 'org-babel-after-execute-hook
            (lambda () (when org-inline-image-overlays (org-redisplay-inline-images))))
  (add-hook 'org-mode-hook 'auto-fill-mode)
  (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)

  (defun org-font-lock-ensure () (font-lock-fontify-buffer)))

;;;; ============================================================================
;;;; PROGRAMMING LANGUAGES
;;;; ============================================================================

;;; LSP (eglot)
(use-package eglot
  :config
  (add-to-list 'eglot-server-programs '(go-mode . ("gopls")))
  (add-to-list 'eglot-server-programs '((c-mode c++-mode) . ("clangd"))))

(use-package eglot-python-preset
  :after eglot
  :custom (eglot-python-preset-lsp-server 'ty)
  :config (eglot-python-preset-setup))

;;; Python
(use-package python-mode
  :mode "\\.py\\'"
  :hook (python-mode . eglot-ensure))
(use-package conda)
(use-package blacken)
(require 'python)

;;; Go
(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . eglot-ensure))

;;; Zig
(use-package zig-mode
  :mode "\\.zig\\'"
  :hook (zig-mode . eglot-ensure))

;;; Web
(use-package web-mode
  :config
  (setq web-mode-markup-indent-offset 2
        web-mode-css-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-style-padding 2
        web-mode-script-padding 2
        web-mode-auto-quote-style 2))

;;; Markdown
(use-package markdown-mode
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown")
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook 'variable-pitch-mode))

;;; BUCK files
(add-to-list 'auto-mode-alist '(".*/BUCK$" . python-mode))

;;; C/C++
(global-set-key (kbd "C-c o") 'ff-find-other-file)

;;;; ============================================================================
;;;; VERSION CONTROL
;;;; ============================================================================

(use-package magit)

(use-package monky
  :config
  (setq monky-process-type 'cmdserver)
  (defun hg-file-history ()
    (interactive)
    (require 'monky)
    (monky-run-hg-async
     "log" "--template"
     "\n{rev}) {date|shortdate}/{author|user}\n{desc|fill68}\n↘\n"
     buffer-file-name)))

(setq vc-follow-symlinks t)

;;;; ============================================================================
;;;; SHELL / TERMINAL
;;;; ============================================================================

(use-package vterm)

;;; Tmux integration
(require 'tmux)
(tmux-setup-keys)

;;;; ============================================================================
;;;; AI / LLM INTEGRATION
;;;; ============================================================================

;;; gptel - LLM chat interface
(use-package gptel
  :config
  (gptel-make-openai "DeepSeek"
    :host "api.deepseek.com"
    :endpoint "/chat/completions"
    :stream t
    :key (getenv "DEEPSEEK_API_KEY")
    :models '(deepseek-chat deepseek-reasoner))
  (gptel-make-anthropic "Claude"
    :stream t
    :key (getenv "ANTHROPIC_API_KEY")))

;;; eca - Emacs Claude Agent
(use-package eca)

;;; agent-shell - Shell interface for AI agents
(use-package agent-shell)

;;;; ============================================================================
;;;; UTILITIES
;;;; ============================================================================

;;; Buffer management
(defun close-all-buffers ()
  "Close all open buffers."
  (interactive)
  (mapc 'kill-buffer (buffer-list)))

(defun revert-all-buffers ()
  "Refresh all open buffers from their files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (buffer-file-name)
        (revert-buffer t t t)))))

;;; Path utility
(defun path ()
  "Copy and display the full path of the current buffer."
  (interactive)
  (kill-new (buffer-file-name))
  (message (buffer-file-name)))

;;; Theme manipulation
(require 'theme)

;;; Writing
(use-package olivetti)

;;;; ============================================================================
;;;; MISCELLANEOUS
;;;; ============================================================================

;;; Compilation
(define-key evil-normal-state-map (kbd "C-c c") 'recompile)

;;; Man pages
(setq Man-notify-method 'pushy)

;;; GDB
(setq gdb-many-windows t)

;;; Dired
(add-hook 'dired-mode-hook (lambda () (dired-hide-details-mode 1)))
(setq dired-use-ls-dired nil)

;;; Bookmarks
(setq bookmark-save-flag 1)

;;;; ============================================================================
;;;; THEMES
;;;; ============================================================================

(use-package poet-theme)
(use-package ef-themes)
(use-package acme-theme)

;;;; ============================================================================
;;;; LOCAL CONFIG
;;;; ============================================================================

(setq local-config (concat user-emacs-directory "local.el"))
(when (file-exists-p local-config)
  (load-file local-config))

(provide 'init)
;;; init.el ends here
