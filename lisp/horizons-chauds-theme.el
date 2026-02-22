;;; horizons-chauds-theme.el --- Dark theme inspired by Wifredo Lam  -*- lexical-binding: t; -*-

;;; Commentary:
;; High-contrast dark theme with a warm palette drawn from
;; "Horizons chauds" (1968) by Wifredo Lam.
;;
;; The painting's bistre browns, ochre golds, scarlet accents,
;; olive greens, and dusty pinks over warm blacks form the basis
;; of this color scheme.

(deftheme horizons-chauds
  "Dark, high-contrast theme inspired by Wifredo Lam's Horizons chauds.")

(let ((class '((class color) (min-colors 89)))
      ;; Background tones - warm blacks
      (bg         "#151010")
      (bg-alt     "#1D1612")
      (bg-hl      "#2A1E16")
      (bg-region  "#3A2818")
      (bg-paren   "#44301E")
      (bg-modeline "#261C14")
      (bg-modeline-inactive "#1A1310")
      ;; Foreground - warm cream
      (fg         "#ECE0CC")
      (fg-dim     "#A09080")
      (fg-faint   "#6A5C4E")
      ;; Palette - from the painting
      (scarlet    "#E04433")   ; keywords, errors
      (amber      "#E09030")   ; functions
      (gold       "#D4A035")   ; strings
      (solar      "#ECC040")   ; cursor, warnings, highlights
      (sienna     "#C87040")   ; builtins, operators
      (pink       "#D0887A")   ; types
      (olive      "#88A050")   ; success, diff add
      (green-dim  "#6E8848")   ; comments
      (bistre     "#8A7050")   ; doc strings, secondary text
      (brown      "#A08058")   ; variables, identifiers
      ;; Structural
      (border     "#3A2C20")
      (hl-line    "#221A14")
      (diff-red   "#3A1815")
      (diff-green "#1A2A15"))

  (custom-theme-set-faces
   'horizons-chauds

   ;; --- Core faces ---
   `(default                          ((,class (:foreground ,fg :background ,bg))))
   `(cursor                           ((,class (:background ,solar))))
   `(region                           ((,class (:background ,bg-region :extend t))))
   `(highlight                        ((,class (:background ,bg-hl))))
   `(hl-line                          ((,class (:background ,hl-line))))
   `(fringe                           ((,class (:background ,bg))))
   `(vertical-border                  ((,class (:foreground ,border))))
   `(border                           ((,class (:foreground ,border))))
   `(shadow                           ((,class (:foreground ,fg-faint))))
   `(minibuffer-prompt                ((,class (:foreground ,amber :bold t))))
   `(link                             ((,class (:foreground ,gold :underline t))))
   `(link-visited                     ((,class (:foreground ,pink :underline t))))
   `(escape-glyph                     ((,class (:foreground ,sienna))))
   `(trailing-whitespace              ((,class (:background ,scarlet))))
   `(lazy-highlight                   ((,class (:background ,bg-paren :foreground ,solar))))
   `(match                            ((,class (:background ,bg-paren :foreground ,gold :bold t))))
   `(secondary-selection              ((,class (:background ,bg-hl))))
   `(success                          ((,class (:foreground ,olive :bold t))))
   `(warning                          ((,class (:foreground ,solar :bold t))))
   `(error                            ((,class (:foreground ,scarlet :bold t))))

   ;; --- Mode line ---
   `(mode-line                        ((,class (:background ,bg-modeline :foreground ,fg
                                                :box (:line-width 4 :color ,bg-modeline)))))
   `(mode-line-inactive               ((,class (:background ,bg-modeline-inactive :foreground ,fg-faint
                                                :box (:line-width 4 :color ,bg-modeline-inactive)))))
   `(mode-line-buffer-id              ((,class (:foreground ,gold :bold t))))
   `(mode-line-emphasis               ((,class (:foreground ,amber :bold t))))
   `(mode-line-highlight              ((,class (:foreground ,solar))))

   ;; --- Font lock (syntax highlighting) ---
   `(font-lock-builtin-face           ((,class (:foreground ,sienna))))
   `(font-lock-comment-face           ((,class (:foreground ,green-dim))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,green-dim))))
   `(font-lock-doc-face               ((,class (:foreground ,bistre))))
   `(font-lock-constant-face          ((,class (:foreground ,gold))))
   `(font-lock-function-name-face     ((,class (:foreground ,amber :bold t))))
   `(font-lock-function-call-face     ((,class (:foreground ,amber))))
   `(font-lock-keyword-face           ((,class (:foreground ,scarlet :bold t))))
   `(font-lock-negation-char-face     ((,class (:foreground ,scarlet))))
   `(font-lock-preprocessor-face      ((,class (:foreground ,sienna))))
   `(font-lock-string-face            ((,class (:foreground ,gold))))
   `(font-lock-type-face              ((,class (:foreground ,pink))))
   `(font-lock-variable-name-face     ((,class (:foreground ,brown))))
   `(font-lock-variable-use-face      ((,class (:foreground ,fg))))
   `(font-lock-warning-face           ((,class (:foreground ,solar :bold t))))
   `(font-lock-number-face            ((,class (:foreground ,gold))))
   `(font-lock-operator-face          ((,class (:foreground ,sienna))))
   `(font-lock-property-name-face     ((,class (:foreground ,brown))))
   `(font-lock-property-use-face      ((,class (:foreground ,fg))))
   `(font-lock-escape-face            ((,class (:foreground ,sienna))))
   `(font-lock-regexp-face            ((,class (:foreground ,gold))))
   `(font-lock-bracket-face           ((,class (:foreground ,fg-dim))))
   `(font-lock-delimiter-face         ((,class (:foreground ,fg-dim))))
   `(font-lock-punctuation-face       ((,class (:foreground ,fg-dim))))
   `(font-lock-misc-punctuation-face  ((,class (:foreground ,fg-dim))))

   ;; --- Search ---
   `(isearch                          ((,class (:background ,solar :foreground ,bg :bold t))))
   `(isearch-fail                     ((,class (:background ,diff-red :foreground ,scarlet))))

   ;; --- Line numbers ---
   `(line-number                      ((,class (:foreground ,fg-faint :background ,bg))))
   `(line-number-current-line         ((,class (:foreground ,gold :background ,hl-line :bold t))))

   ;; --- Paren matching ---
   `(show-paren-match                 ((,class (:background ,bg-paren :foreground ,solar :bold t))))
   `(show-paren-mismatch              ((,class (:background ,scarlet :foreground ,bg :bold t))))

   ;; --- Ivy / Counsel ---
   `(ivy-current-match                ((,class (:background ,bg-region :foreground ,fg :bold t :extend t))))
   `(ivy-minibuffer-match-face-1      ((,class (:foreground ,gold))))
   `(ivy-minibuffer-match-face-2      ((,class (:foreground ,amber :bold t))))
   `(ivy-minibuffer-match-face-3      ((,class (:foreground ,pink))))
   `(ivy-minibuffer-match-face-4      ((,class (:foreground ,olive))))
   `(ivy-confirm-face                 ((,class (:foreground ,olive))))

   ;; --- Company ---
   `(company-tooltip                  ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-common           ((,class (:foreground ,amber :bold t))))
   `(company-tooltip-selection        ((,class (:background ,bg-region))))
   `(company-tooltip-annotation       ((,class (:foreground ,bistre))))
   `(company-scrollbar-bg             ((,class (:background ,bg-alt))))
   `(company-scrollbar-fg             ((,class (:background ,border))))

   ;; --- Magit ---
   `(magit-section-heading            ((,class (:foreground ,amber :bold t))))
   `(magit-section-highlight          ((,class (:background ,hl-line :extend t))))
   `(magit-branch-local               ((,class (:foreground ,gold))))
   `(magit-branch-remote              ((,class (:foreground ,olive))))
   `(magit-diff-added                 ((,class (:background ,diff-green :foreground ,olive :extend t))))
   `(magit-diff-added-highlight       ((,class (:background ,diff-green :foreground ,olive :extend t))))
   `(magit-diff-removed               ((,class (:background ,diff-red :foreground ,scarlet :extend t))))
   `(magit-diff-removed-highlight     ((,class (:background ,diff-red :foreground ,scarlet :extend t))))
   `(magit-diff-context-highlight     ((,class (:background ,hl-line :extend t))))
   `(magit-diff-hunk-heading          ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-region :foreground ,fg :extend t))))
   `(magit-hash                       ((,class (:foreground ,fg-dim))))
   `(magit-log-author                 ((,class (:foreground ,pink))))
   `(magit-log-date                   ((,class (:foreground ,bistre))))

   ;; --- Diff ---
   `(diff-added                       ((,class (:background ,diff-green :foreground ,olive :extend t))))
   `(diff-removed                     ((,class (:background ,diff-red :foreground ,scarlet :extend t))))
   `(diff-header                      ((,class (:background ,bg-alt :foreground ,gold :extend t))))
   `(diff-file-header                 ((,class (:background ,bg-hl :foreground ,amber :bold t :extend t))))
   `(diff-hunk-header                 ((,class (:background ,bg-hl :foreground ,bistre :extend t))))
   `(diff-refine-added                ((,class (:background "#2A3A1A" :foreground ,olive :bold t))))
   `(diff-refine-removed              ((,class (:background "#4A1A15" :foreground ,scarlet :bold t))))

   ;; --- Org mode ---
   `(org-level-1                      ((,class (:foreground ,amber :bold t :height 1.15))))
   `(org-level-2                      ((,class (:foreground ,gold :bold t :height 1.08))))
   `(org-level-3                      ((,class (:foreground ,pink :bold t))))
   `(org-level-4                      ((,class (:foreground ,sienna))))
   `(org-level-5                      ((,class (:foreground ,olive))))
   `(org-level-6                      ((,class (:foreground ,bistre))))
   `(org-document-title               ((,class (:foreground ,solar :bold t :height 1.2))))
   `(org-document-info                ((,class (:foreground ,bistre))))
   `(org-todo                         ((,class (:foreground ,scarlet :bold t))))
   `(org-done                         ((,class (:foreground ,olive :bold t))))
   `(org-date                         ((,class (:foreground ,bistre :underline t))))
   `(org-code                         ((,class (:foreground ,sienna))))
   `(org-verbatim                     ((,class (:foreground ,gold))))
   `(org-block                        ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line             ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-block-end-line               ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-table                        ((,class (:foreground ,brown))))
   `(org-link                         ((,class (:foreground ,gold :underline t))))

   ;; --- Markdown ---
   `(markdown-header-face-1           ((,class (:foreground ,amber :bold t :height 1.15))))
   `(markdown-header-face-2           ((,class (:foreground ,gold :bold t :height 1.08))))
   `(markdown-header-face-3           ((,class (:foreground ,pink :bold t))))
   `(markdown-code-face               ((,class (:foreground ,sienna :background ,bg-alt))))
   `(markdown-inline-code-face        ((,class (:foreground ,sienna :background ,bg-alt))))
   `(markdown-link-face               ((,class (:foreground ,gold))))
   `(markdown-url-face                ((,class (:foreground ,bistre))))

   ;; --- Eglot / Flymake ---
   `(eglot-highlight-symbol-face      ((,class (:background ,bg-hl :bold t))))
   `(flymake-error                    ((,class (:underline (:style wave :color ,scarlet)))))
   `(flymake-warning                  ((,class (:underline (:style wave :color ,solar)))))
   `(flymake-note                     ((,class (:underline (:style wave :color ,olive)))))

   ;; --- Compilation ---
   `(compilation-error                ((,class (:foreground ,scarlet :bold t))))
   `(compilation-warning              ((,class (:foreground ,solar))))
   `(compilation-info                 ((,class (:foreground ,olive))))

   ;; --- Dired ---
   `(dired-directory                  ((,class (:foreground ,amber :bold t))))
   `(dired-symlink                    ((,class (:foreground ,gold))))
   `(dired-ignored                    ((,class (:foreground ,fg-faint))))

   ;; --- Completions ---
   `(completions-common-part          ((,class (:foreground ,amber :bold t))))
   `(completions-first-difference     ((,class (:foreground ,solar))))

   ;; --- Whitespace ---
   `(whitespace-trailing              ((,class (:background ,diff-red))))
   `(whitespace-tab                   ((,class (:foreground ,fg-faint))))
   `(whitespace-space                 ((,class (:foreground ,fg-faint))))

   ;; --- vterm ---
   `(vterm-color-black                ((,class (:foreground ,bg-hl :background ,fg-faint))))
   `(vterm-color-red                  ((,class (:foreground ,scarlet :background ,scarlet))))
   `(vterm-color-green                ((,class (:foreground ,olive :background ,olive))))
   `(vterm-color-yellow               ((,class (:foreground ,gold :background ,gold))))
   `(vterm-color-blue                 ((,class (:foreground ,bistre :background ,bistre))))
   `(vterm-color-magenta              ((,class (:foreground ,pink :background ,pink))))
   `(vterm-color-cyan                 ((,class (:foreground ,green-dim :background ,green-dim))))
   `(vterm-color-white                ((,class (:foreground ,fg :background ,fg)))))

  (custom-theme-set-variables
   'horizons-chauds
   `(ansi-color-names-vector
     [,bg-hl ,scarlet ,olive ,gold ,bistre ,pink ,green-dim ,fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'horizons-chauds)
;;; horizons-chauds-theme.el ends here
