;;; tron-theme.el --- Dark theme inspired by the Tron films  -*- lexical-binding: t; -*-

;;; Commentary:
;; High-contrast dark theme combining the visual language of
;; Tron: Legacy (2010) and Tron: Ares (2025).
;;
;; Legacy's cyan/orange duality meets Ares' RGB three-grid system:
;; ENCOM green, Dillinger red, and Flynn's blue/purple.

(deftheme tron
  "Dark, high-contrast theme inspired by the Tron films.")

(let ((class '((class color) (min-colors 89)))
      ;; Background - the void
      (bg           "#050508")
      (bg-alt       "#0A0C12")
      (bg-hl        "#101420")
      (bg-region    "#152035")
      (bg-paren     "#18304A")
      (bg-modeline  "#080C18")
      (bg-modeline-inactive "#060810")
      ;; Foreground - display phosphor
      (fg           "#D8E0EC")
      (fg-dim       "#607888")
      (fg-faint     "#4A6070")
      ;; Cyan - Legacy hero, Flynn's grid
      (cyan         "#6CF0F0")
      (cyan-bright  "#7DFDFE")
      (cyan-dim     "#2A8898")
      ;; Orange - CLU, Legacy antagonist
      (orange       "#F8B840")
      (orange-dim   "#A07020")
      ;; Red - Dillinger grid (Ares)
      (red          "#E83838")
      (red-dim      "#801818")
      ;; Green - ENCOM grid (Ares)
      (green        "#30E068")
      (green-dim    "#208848")
      ;; Purple - Flynn's retro grid (Ares)
      (purple       "#A060F0")
      (purple-dim   "#604090")
      ;; Blue - deep grid accents
      (blue         "#3088E8")
      (blue-deep    "#0A2060")
      ;; White - transfer portals
      (white        "#F0F4FF")
      ;; Structural
      (border       "#182838")
      (hl-line      "#080C16")
      (grid-line    "#101828")
      (diff-red-bg  "#200A0A")
      (diff-green-bg "#0A200A"))

  (custom-theme-set-faces
   'tron

   ;; --- Core faces ---
   `(default                          ((,class (:foreground ,fg :background ,bg))))
   `(cursor                           ((,class (:background ,cyan-bright))))
   `(region                           ((,class (:background ,bg-region :extend t))))
   `(highlight                        ((,class (:background ,bg-hl))))
   `(hl-line                          ((,class (:background ,hl-line))))
   `(fringe                           ((,class (:background ,bg))))
   `(vertical-border                  ((,class (:foreground ,border))))
   `(border                           ((,class (:foreground ,border))))
   `(shadow                           ((,class (:foreground ,fg-faint))))
   `(minibuffer-prompt                ((,class (:foreground ,cyan :bold t))))
   `(link                             ((,class (:foreground ,cyan :underline t))))
   `(link-visited                     ((,class (:foreground ,purple :underline t))))
   `(escape-glyph                     ((,class (:foreground ,orange))))
   `(trailing-whitespace              ((,class (:background ,red))))
   `(lazy-highlight                   ((,class (:background ,bg-paren :foreground ,cyan-bright))))
   `(match                            ((,class (:background ,bg-paren :foreground ,cyan :bold t))))
   `(secondary-selection              ((,class (:background ,bg-hl))))
   `(success                          ((,class (:foreground ,green :bold t))))
   `(warning                          ((,class (:foreground ,orange :bold t))))
   `(error                            ((,class (:foreground ,red :bold t))))

   ;; --- Mode line ---
   `(mode-line                        ((,class (:background ,bg-modeline :foreground ,cyan
                                                :box (:line-width 4 :color ,bg-modeline)))))
   `(mode-line-inactive               ((,class (:background ,bg-modeline-inactive :foreground ,fg-faint
                                                :box (:line-width 4 :color ,bg-modeline-inactive)))))
   `(mode-line-buffer-id              ((,class (:foreground ,cyan-bright :bold t))))
   `(mode-line-emphasis               ((,class (:foreground ,orange :bold t))))
   `(mode-line-highlight              ((,class (:foreground ,white))))

   ;; --- Font lock (syntax highlighting) ---
   `(font-lock-builtin-face           ((,class (:foreground ,purple))))
   `(font-lock-comment-face           ((,class (:foreground ,fg-faint))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-faint))))
   `(font-lock-doc-face               ((,class (:foreground ,cyan-dim))))
   `(font-lock-constant-face          ((,class (:foreground ,orange))))
   `(font-lock-function-name-face     ((,class (:foreground ,cyan :bold t))))
   `(font-lock-function-call-face     ((,class (:foreground ,cyan))))
   `(font-lock-keyword-face           ((,class (:foreground ,blue :bold t))))
   `(font-lock-negation-char-face     ((,class (:foreground ,red))))
   `(font-lock-preprocessor-face      ((,class (:foreground ,purple))))
   `(font-lock-string-face            ((,class (:foreground ,green))))
   `(font-lock-type-face              ((,class (:foreground ,orange))))
   `(font-lock-variable-name-face     ((,class (:foreground ,fg))))
   `(font-lock-variable-use-face      ((,class (:foreground ,fg))))
   `(font-lock-warning-face           ((,class (:foreground ,red :bold t))))
   `(font-lock-number-face            ((,class (:foreground ,orange))))
   `(font-lock-operator-face          ((,class (:foreground ,cyan-dim))))
   `(font-lock-property-name-face     ((,class (:foreground ,fg-dim))))
   `(font-lock-property-use-face      ((,class (:foreground ,fg))))
   `(font-lock-escape-face            ((,class (:foreground ,purple))))
   `(font-lock-regexp-face            ((,class (:foreground ,green))))
   `(font-lock-bracket-face           ((,class (:foreground ,fg-dim))))
   `(font-lock-delimiter-face         ((,class (:foreground ,fg-dim))))
   `(font-lock-punctuation-face       ((,class (:foreground ,fg-dim))))
   `(font-lock-misc-punctuation-face  ((,class (:foreground ,fg-dim))))

   ;; --- Search ---
   `(isearch                          ((,class (:background ,cyan :foreground ,bg :bold t))))
   `(isearch-fail                     ((,class (:background ,diff-red-bg :foreground ,red))))

   ;; --- Line numbers ---
   `(line-number                      ((,class (:foreground ,fg-faint :background ,bg))))
   `(line-number-current-line         ((,class (:foreground ,cyan :background ,hl-line :bold t))))

   ;; --- Paren matching ---
   `(show-paren-match                 ((,class (:background ,bg-paren :foreground ,cyan-bright :bold t))))
   `(show-paren-mismatch              ((,class (:background ,red :foreground ,bg :bold t))))

   ;; --- Ivy / Counsel ---
   `(ivy-current-match                ((,class (:background ,bg-region :foreground ,fg :bold t :extend t))))
   `(ivy-minibuffer-match-face-1      ((,class (:foreground ,cyan))))
   `(ivy-minibuffer-match-face-2      ((,class (:foreground ,green :bold t))))
   `(ivy-minibuffer-match-face-3      ((,class (:foreground ,orange))))
   `(ivy-minibuffer-match-face-4      ((,class (:foreground ,purple))))
   `(ivy-confirm-face                 ((,class (:foreground ,green))))

   ;; --- Company ---
   `(company-tooltip                  ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-common           ((,class (:foreground ,cyan :bold t))))
   `(company-tooltip-selection        ((,class (:background ,bg-region))))
   `(company-tooltip-annotation       ((,class (:foreground ,fg-dim))))
   `(company-scrollbar-bg             ((,class (:background ,bg-alt))))
   `(company-scrollbar-fg             ((,class (:background ,border))))

   ;; --- Magit ---
   `(magit-section-heading            ((,class (:foreground ,cyan :bold t))))
   `(magit-section-highlight          ((,class (:background ,hl-line :extend t))))
   `(magit-branch-local               ((,class (:foreground ,cyan))))
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
   `(magit-log-date                   ((,class (:foreground ,cyan-dim))))

   ;; --- Diff ---
   `(diff-added                       ((,class (:background ,diff-green-bg :foreground ,green :extend t))))
   `(diff-removed                     ((,class (:background ,diff-red-bg :foreground ,red :extend t))))
   `(diff-header                      ((,class (:background ,bg-alt :foreground ,cyan :extend t))))
   `(diff-file-header                 ((,class (:background ,bg-hl :foreground ,cyan :bold t :extend t))))
   `(diff-hunk-header                 ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(diff-refine-added                ((,class (:background "#0A3010" :foreground ,green :bold t))))
   `(diff-refine-removed              ((,class (:background "#3A0A0A" :foreground ,red :bold t))))

   ;; --- Org mode ---
   `(org-level-1                      ((,class (:foreground ,cyan :bold t :height 1.15))))
   `(org-level-2                      ((,class (:foreground ,blue :bold t :height 1.08))))
   `(org-level-3                      ((,class (:foreground ,purple :bold t))))
   `(org-level-4                      ((,class (:foreground ,orange))))
   `(org-level-5                      ((,class (:foreground ,green))))
   `(org-level-6                      ((,class (:foreground ,cyan-dim))))
   `(org-document-title               ((,class (:foreground ,cyan-bright :bold t :height 1.2))))
   `(org-document-info                ((,class (:foreground ,fg-dim))))
   `(org-todo                         ((,class (:foreground ,red :bold t))))
   `(org-done                         ((,class (:foreground ,green :bold t))))
   `(org-date                         ((,class (:foreground ,cyan-dim :underline t))))
   `(org-code                         ((,class (:foreground ,purple))))
   `(org-verbatim                     ((,class (:foreground ,green))))
   `(org-block                        ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line             ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-block-end-line               ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-table                        ((,class (:foreground ,cyan-dim))))
   `(org-link                         ((,class (:foreground ,cyan :underline t))))

   ;; --- Markdown ---
   `(markdown-header-face-1           ((,class (:foreground ,cyan :bold t :height 1.15))))
   `(markdown-header-face-2           ((,class (:foreground ,blue :bold t :height 1.08))))
   `(markdown-header-face-3           ((,class (:foreground ,purple :bold t))))
   `(markdown-code-face               ((,class (:foreground ,green :background ,bg-alt))))
   `(markdown-inline-code-face        ((,class (:foreground ,green :background ,bg-alt))))
   `(markdown-link-face               ((,class (:foreground ,cyan))))
   `(markdown-url-face                ((,class (:foreground ,fg-dim))))

   ;; --- Eglot / Flymake ---
   `(eglot-highlight-symbol-face      ((,class (:background ,bg-hl :bold t))))
   `(flymake-error                    ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning                  ((,class (:underline (:style wave :color ,orange)))))
   `(flymake-note                     ((,class (:underline (:style wave :color ,green)))))

   ;; --- Compilation ---
   `(compilation-error                ((,class (:foreground ,red :bold t))))
   `(compilation-warning              ((,class (:foreground ,orange))))
   `(compilation-info                 ((,class (:foreground ,green))))

   ;; --- Dired ---
   `(dired-directory                  ((,class (:foreground ,cyan :bold t))))
   `(dired-symlink                    ((,class (:foreground ,purple))))
   `(dired-ignored                    ((,class (:foreground ,fg-faint))))

   ;; --- Completions ---
   `(completions-common-part          ((,class (:foreground ,cyan :bold t))))
   `(completions-first-difference     ((,class (:foreground ,green))))

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
   'tron
   `(ansi-color-names-vector
     [,bg-hl ,red ,green ,orange ,blue ,purple ,cyan ,fg])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'tron)
;;; tron-theme.el ends here
