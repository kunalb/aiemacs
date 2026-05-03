;;; early-init.el --- Early Emacs initialization  -*- lexical-binding: t; -*-

;;; Commentary:
;; Settings here must run before Emacs loads init.el.

;;; Code:

;; Prefer init.el over init.elc whenever the source is newer, so stale
;; bytecode cannot shadow recent config edits.
(setq load-prefer-newer t)

(provide 'early-init)
;;; early-init.el ends here
