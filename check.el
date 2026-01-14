;;; check.el --- Sanity checks for aiemacs config  -*- lexical-binding: t; -*-

;;; Commentary:
;; Run with: emacs -Q --batch -l check.el

;;; Code:

(require 'seq)

(defvar check-builtin-features
  '(python ruby perl javascript js cc-mode c-mode c++-mode
    org org-mode dired eshell shell tramp eglot flymake
    project xref eldoc compile grep find-dired info man
    calendar diary bookmark desktop recentf savehist
    abbrev hippie-exp dabbrev tempo skeleton)
  "Features that are built-in to Emacs and should not be shadowed.")

(defvar check-errors nil
  "List of errors found during checks.")

(defun check-add-error (fmt &rest args)
  "Add an error message formatted with FMT and ARGS."
  (push (apply #'format fmt args) check-errors))

(defun check-shadowed-builtins ()
  "Check if any lisp/*.el files shadow built-in packages."
  (let ((lisp-dir (expand-file-name "lisp" (file-name-directory load-file-name))))
    (when (file-directory-p lisp-dir)
      (dolist (file (directory-files lisp-dir t "\\.el$"))
        (let ((feature (intern (file-name-sans-extension (file-name-nondirectory file)))))
          (when (memq feature check-builtin-features)
            (check-add-error "SHADOW: lisp/%s.el shadows built-in '%s'"
                             feature feature))
          ;; Also check what the file actually provides
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (while (re-search-forward "(provide '\\([^)]+\\))" nil t)
              (let ((provided (intern (match-string 1))))
                (when (and (memq provided check-builtin-features)
                           (not (eq provided feature)))
                  (check-add-error "SHADOW: lisp/%s.el provides '%s' which shadows built-in"
                                   (file-name-nondirectory file) provided))))))))))

(defun check-provide-matches-filename ()
  "Check that each lisp/*.el file's provide matches its filename."
  (let ((lisp-dir (expand-file-name "lisp" (file-name-directory load-file-name))))
    (when (file-directory-p lisp-dir)
      (dolist (file (directory-files lisp-dir t "\\.el$"))
        (let ((expected-feature (file-name-sans-extension (file-name-nondirectory file)))
              (provides nil))
          (with-temp-buffer
            (insert-file-contents file)
            (goto-char (point-min))
            (when (re-search-forward "(provide '\\([^)]+\\))" nil t)
              (setq provides (match-string 1))))
          (when (and provides (not (string= provides expected-feature)))
            (check-add-error "MISMATCH: lisp/%s provides '%s' (expected '%s')"
                             (file-name-nondirectory file) provides expected-feature)))))))

(defun check-byte-compile ()
  "Try to byte-compile all .el files."
  (let* ((base-dir (file-name-directory load-file-name))
         (files (append
                 (list (expand-file-name "init.el" base-dir))
                 (directory-files (expand-file-name "lisp" base-dir) t "\\.el$"))))
    (dolist (file files)
      (condition-case err
          (let ((byte-compile-warnings '(not free-vars unresolved)))
            (unless (byte-compile-file file)
              (check-add-error "COMPILE: %s failed to byte-compile"
                               (file-name-nondirectory file))))
        (error
         (check-add-error "COMPILE: %s - %s"
                          (file-name-nondirectory file)
                          (error-message-string err)))))))

(defun check-duplicate-keybindings ()
  "Check for duplicate keybindings in init.el."
  (let ((init-file (expand-file-name "init.el" (file-name-directory load-file-name)))
        (bindings (make-hash-table :test 'equal)))
    (with-temp-buffer
      (insert-file-contents init-file)
      (goto-char (point-min))
      (while (re-search-forward "(global-set-key (kbd \"\\([^\"]+\\)\")" nil t)
        (let ((key (match-string 1)))
          (if (gethash key bindings)
              (check-add-error "DUPLICATE KEY: %s bound multiple times" key)
            (puthash key t bindings)))))))

(defun check-run-all ()
  "Run all sanity checks."
  (setq check-errors nil)
  (message "Running sanity checks...")
  (message "  Checking for shadowed built-ins...")
  (check-shadowed-builtins)
  (message "  Checking provide/filename match...")
  (check-provide-matches-filename)
  (message "  Checking for duplicate keybindings...")
  (check-duplicate-keybindings)
  ;; Byte-compile is slow, skip by default
  ;; (message "  Byte-compiling...")
  ;; (check-byte-compile)
  (if check-errors
      (progn
        (message "\nErrors found:")
        (dolist (err (reverse check-errors))
          (message "  %s" err))
        (kill-emacs 1))
    (message "\nAll checks passed!")
    (kill-emacs 0)))

;; Run checks when loaded in batch mode
(when noninteractive
  (check-run-all))

(provide 'check)
;;; check.el ends here
