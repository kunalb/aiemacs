;;; python.el --- Python environment helpers  -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for working with Python virtual environments,
;; particularly those managed by uv.

(defun uv-activate (&optional arg)
  "Activate Python environment managed by uv.
Looks for .venv in project root. With prefix ARG, prompt for path."
  (interactive "P")
  (let* ((project-root (when-let ((proj (project-current)))
                         (project-root proj)))
         (default-root (or project-root default-directory))
         (venv-path (if arg
                        (read-directory-name "Select venv directory: " default-root)
                      (if project-root
                          (expand-file-name ".venv" project-root)
                        (error "Not in a project; use C-u to specify venv path"))))
         (python-exe (if (eq system-type 'windows-nt) "Scripts/python.exe" "bin/python"))
         (python-path (expand-file-name python-exe venv-path)))
    (if (file-exists-p python-path)
        (progn
          (setq-local python-shell-interpreter python-path)
          (setq-local python-shell-virtualenv-root venv-path)
          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq-local exec-path (cons venv-bin-dir (remove venv-bin-dir exec-path))))
          (setq-local process-environment (copy-sequence process-environment))
          (setenv "PATH" (concat (file-name-directory python-path) path-separator (getenv "PATH")))
          (setenv "VIRTUAL_ENV" venv-path)
          (setenv "PYTHONHOME" nil)
          (message "Activated venv: %s" venv-path))
      (error "No venv found at %s" venv-path))))

(provide 'python)
;;; python.el ends here
