;;; tron-red-theme.el --- Dark theme inspired by the Dillinger Grid  -*- lexical-binding: t; -*-

;;; Commentary:
;; High-contrast dark theme based on the Dillinger Grid from
;; Tron: Ares (2025) — red neon on black, industrial and aggressive.

(deftheme tron-red
  "Dark, high-contrast theme inspired by the Tron Dillinger Grid.")

(let ((class '((class color) (min-colors 89)))
      ;; Background - the void
      (bg           "#080505")
      (bg-alt       "#120A0A")
      (bg-hl        "#201010")
      (bg-region    "#351818")
      (bg-paren     "#4A1820")
      (bg-modeline  "#180808")
      (bg-modeline-inactive "#100606")
      ;; Foreground - display phosphor
      (fg           "#ECE0D8")
      (fg-dim       "#887060")
      (fg-faint     "#706058")
      ;; Red - Dillinger Grid primary
      (red          "#F04040")
      (red-bright   "#FF5050")
      (red-dim      "#983030")
      ;; Orange - CLU legacy
      (orange       "#F8B840")
      (orange-dim   "#A07020")
      ;; Cyan - traces of the old grid
      (cyan         "#50C8D0")
      (cyan-dim     "#2A8898")
      ;; Green - ENCOM grid
      (green        "#30E068")
      (green-dim    "#208848")
      ;; Purple - Flynn's grid
      (purple       "#A060F0")
      (purple-dim   "#604090")
      ;; Blue - deep accents
      (blue         "#3088E8")
      ;; Warm tones
      (amber        "#FF9020")
      (pink         "#E06880")
      ;; White
      (white        "#F0F4FF")
      ;; Structural
      (border       "#382020")
      (hl-line      "#160A0A")
      (diff-red-bg  "#200A0A")
      (diff-green-bg "#0A200A"))

  (custom-theme-set-faces
   'tron-red

   ;; --- Core faces ---
   `(default                          ((,class (:foreground ,fg :background ,bg))))
   `(cursor                           ((,class (:background ,red-bright))))
   `(region                           ((,class (:background ,bg-region :extend t))))
   `(highlight                        ((,class (:background ,bg-hl))))
   `(hl-line                          ((,class (:background ,hl-line))))
   `(fringe                           ((,class (:background ,bg))))
   `(vertical-border                  ((,class (:foreground ,border))))
   `(border                           ((,class (:foreground ,border))))
   `(shadow                           ((,class (:foreground ,fg-faint))))
   `(minibuffer-prompt                ((,class (:foreground ,red :bold t))))
   `(link                             ((,class (:foreground ,red :underline t))))
   `(link-visited                     ((,class (:foreground ,purple :underline t))))
   `(escape-glyph                     ((,class (:foreground ,orange))))
   `(trailing-whitespace              ((,class (:background ,red-bright))))
   `(lazy-highlight                   ((,class (:background ,bg-paren :foreground ,red-bright))))
   `(match                            ((,class (:background ,bg-paren :foreground ,red :bold t))))
   `(secondary-selection              ((,class (:background ,bg-hl))))
   `(success                          ((,class (:foreground ,green :bold t))))
   `(warning                          ((,class (:foreground ,orange :bold t))))
   `(error                            ((,class (:foreground ,red-bright :bold t))))

   ;; --- Mode line ---
   `(mode-line                        ((,class (:background ,bg-modeline :foreground ,red
                                                :box (:line-width 4 :color ,bg-modeline)))))
   `(mode-line-inactive               ((,class (:background ,bg-modeline-inactive :foreground ,fg-faint
                                                :box (:line-width 4 :color ,bg-modeline-inactive)))))
   `(mode-line-buffer-id              ((,class (:foreground ,red-bright :bold t))))
   `(mode-line-emphasis               ((,class (:foreground ,orange :bold t))))
   `(mode-line-highlight              ((,class (:foreground ,white))))

   ;; --- Font lock (syntax highlighting) ---
   `(font-lock-builtin-face           ((,class (:foreground ,purple))))
   `(font-lock-comment-face           ((,class (:foreground ,fg-faint))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-faint))))
   `(font-lock-doc-face               ((,class (:foreground ,red-dim))))
   `(font-lock-constant-face          ((,class (:foreground ,orange))))
   `(font-lock-function-name-face     ((,class (:foreground ,red :bold t))))
   `(font-lock-function-call-face     ((,class (:foreground ,red))))
   `(font-lock-keyword-face           ((,class (:foreground ,amber :bold t))))
   `(font-lock-negation-char-face     ((,class (:foreground ,red-bright))))
   `(font-lock-preprocessor-face      ((,class (:foreground ,purple))))
   `(font-lock-string-face            ((,class (:foreground ,cyan))))
   `(font-lock-type-face              ((,class (:foreground ,orange))))
   `(font-lock-variable-name-face     ((,class (:foreground ,fg))))
   `(font-lock-variable-use-face      ((,class (:foreground ,fg))))
   `(font-lock-warning-face           ((,class (:foreground ,red-bright :bold t))))
   `(font-lock-number-face            ((,class (:foreground ,orange))))
   `(font-lock-operator-face          ((,class (:foreground ,red-dim))))
   `(font-lock-property-name-face     ((,class (:foreground ,fg-dim))))
   `(font-lock-property-use-face      ((,class (:foreground ,fg))))
   `(font-lock-escape-face            ((,class (:foreground ,purple))))
   `(font-lock-regexp-face            ((,class (:foreground ,cyan))))
   `(font-lock-bracket-face           ((,class (:foreground ,fg-dim))))
   `(font-lock-delimiter-face         ((,class (:foreground ,fg-dim))))
   `(font-lock-punctuation-face       ((,class (:foreground ,fg-dim))))
   `(font-lock-misc-punctuation-face  ((,class (:foreground ,fg-dim))))

   ;; --- Search ---
   `(isearch                          ((,class (:background ,red :foreground ,bg :bold t))))
   `(isearch-fail                     ((,class (:background ,diff-red-bg :foreground ,red-bright))))

   ;; --- Line numbers ---
   `(line-number                      ((,class (:foreground ,fg-faint :background ,bg))))
   `(line-number-current-line         ((,class (:foreground ,red :background ,hl-line :bold t))))

   ;; --- Paren matching ---
   `(show-paren-match                 ((,class (:background ,bg-paren :foreground ,red-bright :bold t))))
   `(show-paren-mismatch              ((,class (:background ,orange :foreground ,bg :bold t))))

   ;; --- Ivy / Counsel ---
   `(ivy-current-match                ((,class (:background ,bg-region :foreground ,fg :bold t :extend t))))
   `(ivy-minibuffer-match-face-1      ((,class (:foreground ,red))))
   `(ivy-minibuffer-match-face-2      ((,class (:foreground ,orange :bold t))))
   `(ivy-minibuffer-match-face-3      ((,class (:foreground ,cyan))))
   `(ivy-minibuffer-match-face-4      ((,class (:foreground ,purple))))
   `(ivy-confirm-face                 ((,class (:foreground ,green))))

   ;; --- Company ---
   `(company-tooltip                  ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-common           ((,class (:foreground ,red :bold t))))
   `(company-tooltip-selection        ((,class (:background ,bg-region))))
   `(company-tooltip-annotation       ((,class (:foreground ,fg-dim))))
   `(company-scrollbar-bg             ((,class (:background ,bg-alt))))
   `(company-scrollbar-fg             ((,class (:background ,border))))

   ;; --- Magit ---
   `(magit-section-heading            ((,class (:foreground ,red :bold t))))
   `(magit-section-highlight          ((,class (:background ,hl-line :extend t))))
   `(magit-branch-local               ((,class (:foreground ,red))))
   `(magit-branch-remote              ((,class (:foreground ,green))))
   `(magit-diff-added                 ((,class (:background ,diff-green-bg :foreground ,green :extend t))))
   `(magit-diff-added-highlight       ((,class (:background ,diff-green-bg :foreground ,green :extend t))))
   `(magit-diff-removed               ((,class (:background ,diff-red-bg :foreground ,red :extend t))))
   `(magit-diff-removed-highlight     ((,class (:background ,diff-red-bg :foreground ,red :extend t))))
   `(magit-diff-context-highlight     ((,class (:background ,hl-line :extend t))))
   `(magit-diff-hunk-heading          ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-region :foreground ,fg :extend t))))
   `(magit-hash                       ((,class (:foreground ,fg-dim))))
   `(magit-log-author                 ((,class (:foreground ,orange))))
   `(magit-log-date                   ((,class (:foreground ,red-dim))))

   ;; --- Diff ---
   `(diff-added                       ((,class (:background ,diff-green-bg :foreground ,green :extend t))))
   `(diff-removed                     ((,class (:background ,diff-red-bg :foreground ,red :extend t))))
   `(diff-header                      ((,class (:background ,bg-alt :foreground ,red :extend t))))
   `(diff-file-header                 ((,class (:background ,bg-hl :foreground ,red :bold t :extend t))))
   `(diff-hunk-header                 ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(diff-refine-added                ((,class (:background "#0A3010" :foreground ,green :bold t))))
   `(diff-refine-removed              ((,class (:background "#3A0A0A" :foreground ,red-bright :bold t))))

   ;; --- Org mode ---
   `(org-level-1                      ((,class (:foreground ,red :bold t :height 1.15))))
   `(org-level-2                      ((,class (:foreground ,amber :bold t :height 1.08))))
   `(org-level-3                      ((,class (:foreground ,purple :bold t))))
   `(org-level-4                      ((,class (:foreground ,orange))))
   `(org-level-5                      ((,class (:foreground ,cyan))))
   `(org-level-6                      ((,class (:foreground ,red-dim))))
   `(org-document-title               ((,class (:foreground ,red-bright :bold t :height 1.2))))
   `(org-document-info                ((,class (:foreground ,fg-dim))))
   `(org-todo                         ((,class (:foreground ,red-bright :bold t))))
   `(org-done                         ((,class (:foreground ,green :bold t))))
   `(org-date                         ((,class (:foreground ,red-dim :underline t))))
   `(org-code                         ((,class (:foreground ,purple))))
   `(org-verbatim                     ((,class (:foreground ,cyan))))
   `(org-block                        ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line             ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-block-end-line               ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-table                        ((,class (:foreground ,red-dim))))
   `(org-link                         ((,class (:foreground ,red :underline t))))

   ;; --- Markdown ---
   `(markdown-header-face-1           ((,class (:foreground ,red :bold t :height 1.15))))
   `(markdown-header-face-2           ((,class (:foreground ,amber :bold t :height 1.08))))
   `(markdown-header-face-3           ((,class (:foreground ,purple :bold t))))
   `(markdown-code-face               ((,class (:foreground ,cyan :background ,bg-alt))))
   `(markdown-inline-code-face        ((,class (:foreground ,cyan :background ,bg-alt))))
   `(markdown-link-face               ((,class (:foreground ,red))))
   `(markdown-url-face                ((,class (:foreground ,fg-dim))))

   ;; --- Eglot / Flymake ---
   `(eglot-highlight-symbol-face      ((,class (:background ,bg-hl :bold t))))
   `(flymake-error                    ((,class (:underline (:style wave :color ,red-bright)))))
   `(flymake-warning                  ((,class (:underline (:style wave :color ,orange)))))
   `(flymake-note                     ((,class (:underline (:style wave :color ,green)))))

   ;; --- Compilation ---
   `(compilation-error                ((,class (:foreground ,red-bright :bold t))))
   `(compilation-warning              ((,class (:foreground ,orange))))
   `(compilation-info                 ((,class (:foreground ,green))))

   ;; --- Dired ---
   `(dired-directory                  ((,class (:foreground ,red :bold t))))
   `(dired-symlink                    ((,class (:foreground ,purple))))
   `(dired-ignored                    ((,class (:foreground ,fg-faint))))

   ;; --- Completions ---
   `(completions-common-part          ((,class (:foreground ,red :bold t))))
   `(completions-first-difference     ((,class (:foreground ,orange))))

   ;; --- Whitespace ---
   `(whitespace-trailing              ((,class (:background ,diff-red-bg))))
   `(whitespace-tab                   ((,class (:foreground ,fg-faint))))
   `(whitespace-space                 ((,class (:foreground ,fg-faint))))

   ;; --- vterm ---
   `(vterm-color-black                ((,class (:foreground ,bg-hl :background ,fg-faint))))
   `(vterm-color-red                  ((,class (:foreground ,red :background ,red))))
   `(vterm-color-green                ((,class (:foreground ,green :background ,green))))
   `(vterm-color-yellow               ((,class (:foreground ,orange :background ,orange))))
   `(vterm-color-blue                 ((,class (:foreground ,blue :background ,blue))))
   `(vterm-color-magenta              ((,class (:foreground ,purple :background ,purple))))
   `(vterm-color-cyan                 ((,class (:foreground ,cyan :background ,cyan))))
   `(vterm-color-white                ((,class (:foreground ,fg :background ,fg)))))

  (custom-theme-set-variables
   'tron-red
   `(ansi-color-names-vector
     [,bg-hl ,red ,green ,orange ,blue ,purple ,cyan ,fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'tron-red)
;;; tron-red-theme.el ends here
