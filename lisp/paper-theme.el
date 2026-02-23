;;; paper-theme.el --- Minimal light theme with red accents  -*- lexical-binding: t; -*-

;;; Commentary:
;; A clean, high-contrast light theme for programming.
;; Mostly black text on warm cream, with red accents and subtle
;; tones drawn from Kandinsky's Accompanied Contrast (1935).

(deftheme paper
  "Minimal light theme with red accents and mostly black text.")

(let ((class '((class color) (min-colors 89)))
      ;; Background
      (bg          "#FEFCF8")
      (bg-alt      "#F5F2EC")
      (bg-hl       "#EDE9E2")
      (bg-region   "#E0DCD4")
      (bg-paren    "#D8D2C8")
      (bg-modeline "#EDEAE4")
      (bg-modeline-inactive "#F5F2EC")
      ;; Foreground - near black
      (fg          "#1A1A1A")
      (fg-dim      "#5A5A5A")
      (fg-faint    "#787068")
      ;; Red - primary accent
      (red         "#C02020")
      (red-bright  "#D83030")
      (red-dim     "#8A2020")
      ;; Kandinsky tones
      (navy        "#2A4068")
      (mauve       "#884868")
      (brown       "#7A5030")
      (olive       "#3E6420")
      (plum        "#6A3868")
      ;; Structural
      (border      "#D0CCC4")
      (hl-line     "#F5F0E8")
      (diff-red    "#FCDCDC")
      (diff-green  "#DCF0DC"))

  (custom-theme-set-faces
   'paper

   ;; --- Core faces ---
   `(default                          ((,class (:foreground ,fg :background ,bg))))
   `(cursor                           ((,class (:background ,red))))
   `(region                           ((,class (:background ,bg-region :extend t))))
   `(highlight                        ((,class (:background ,bg-hl))))
   `(hl-line                          ((,class (:background ,hl-line))))
   `(fringe                           ((,class (:background ,bg))))
   `(vertical-border                  ((,class (:foreground ,border))))
   `(border                           ((,class (:foreground ,border))))
   `(shadow                           ((,class (:foreground ,fg-faint))))
   `(minibuffer-prompt                ((,class (:foreground ,red :bold t))))
   `(link                             ((,class (:foreground ,red-dim :underline t))))
   `(link-visited                     ((,class (:foreground ,plum :underline t))))
   `(escape-glyph                     ((,class (:foreground ,brown))))
   `(trailing-whitespace              ((,class (:background ,diff-red))))
   `(lazy-highlight                   ((,class (:background ,bg-paren))))
   `(match                            ((,class (:background ,bg-paren :bold t))))
   `(secondary-selection              ((,class (:background ,bg-hl))))
   `(success                          ((,class (:foreground ,olive :bold t))))
   `(warning                          ((,class (:foreground ,brown :bold t))))
   `(error                            ((,class (:foreground ,red :bold t))))

   ;; --- Mode line ---
   `(mode-line                        ((,class (:background ,bg-modeline :foreground ,fg
                                                :box (:line-width 4 :color ,bg-modeline)))))
   `(mode-line-inactive               ((,class (:background ,bg-modeline-inactive :foreground ,fg-faint
                                                :box (:line-width 4 :color ,bg-modeline-inactive)))))
   `(mode-line-buffer-id              ((,class (:foreground ,fg :bold t))))
   `(mode-line-emphasis               ((,class (:foreground ,red :bold t))))
   `(mode-line-highlight              ((,class (:foreground ,red))))

   ;; --- Font lock (syntax highlighting) ---
   `(font-lock-builtin-face           ((,class (:foreground ,mauve))))
   `(font-lock-comment-face           ((,class (:foreground ,fg-faint))))
   `(font-lock-comment-delimiter-face ((,class (:foreground ,fg-faint))))
   `(font-lock-doc-face               ((,class (:foreground ,fg-dim))))
   `(font-lock-constant-face          ((,class (:foreground ,navy :bold t))))
   `(font-lock-function-name-face     ((,class (:foreground ,fg :bold t))))
   `(font-lock-function-call-face     ((,class (:foreground ,fg))))
   `(font-lock-keyword-face           ((,class (:foreground ,red :bold t))))
   `(font-lock-negation-char-face     ((,class (:foreground ,red))))
   `(font-lock-preprocessor-face      ((,class (:foreground ,mauve))))
   `(font-lock-string-face            ((,class (:foreground ,olive))))
   `(font-lock-type-face              ((,class (:foreground ,navy))))
   `(font-lock-variable-name-face     ((,class (:foreground ,fg))))
   `(font-lock-variable-use-face      ((,class (:foreground ,fg))))
   `(font-lock-warning-face           ((,class (:foreground ,red :bold t))))
   `(font-lock-number-face            ((,class (:foreground ,brown))))
   `(font-lock-operator-face          ((,class (:foreground ,fg))))
   `(font-lock-property-name-face     ((,class (:foreground ,fg))))
   `(font-lock-property-use-face      ((,class (:foreground ,fg))))
   `(font-lock-escape-face            ((,class (:foreground ,mauve))))
   `(font-lock-regexp-face            ((,class (:foreground ,brown))))
   `(font-lock-bracket-face           ((,class (:foreground ,fg-dim))))
   `(font-lock-delimiter-face         ((,class (:foreground ,fg-dim))))
   `(font-lock-punctuation-face       ((,class (:foreground ,fg-dim))))
   `(font-lock-misc-punctuation-face  ((,class (:foreground ,fg-dim))))

   ;; --- Search ---
   `(isearch                          ((,class (:background ,red :foreground ,bg :bold t))))
   `(isearch-fail                     ((,class (:background ,diff-red :foreground ,red))))

   ;; --- Line numbers ---
   `(line-number                      ((,class (:foreground ,fg-faint :background ,bg))))
   `(line-number-current-line         ((,class (:foreground ,fg :background ,hl-line :bold t))))

   ;; --- Paren matching ---
   `(show-paren-match                 ((,class (:background ,bg-paren :bold t))))
   `(show-paren-mismatch              ((,class (:background ,red :foreground ,bg :bold t))))

   ;; --- Ivy / Counsel ---
   `(ivy-current-match                ((,class (:background ,bg-region :foreground ,fg :bold t :extend t))))
   `(ivy-minibuffer-match-face-1      ((,class (:foreground ,red))))
   `(ivy-minibuffer-match-face-2      ((,class (:foreground ,red-dim :bold t))))
   `(ivy-minibuffer-match-face-3      ((,class (:foreground ,brown))))
   `(ivy-minibuffer-match-face-4      ((,class (:foreground ,olive))))
   `(ivy-confirm-face                 ((,class (:foreground ,olive))))

   ;; --- Company ---
   `(company-tooltip                  ((,class (:background ,bg-alt :foreground ,fg))))
   `(company-tooltip-common           ((,class (:foreground ,red :bold t))))
   `(company-tooltip-selection        ((,class (:background ,bg-region))))
   `(company-tooltip-annotation       ((,class (:foreground ,fg-dim))))
   `(company-scrollbar-bg             ((,class (:background ,bg-alt))))
   `(company-scrollbar-fg             ((,class (:background ,border))))

   ;; --- Magit ---
   `(magit-section-heading            ((,class (:foreground ,fg :bold t))))
   `(magit-section-highlight          ((,class (:background ,hl-line :extend t))))
   `(magit-branch-local               ((,class (:foreground ,red))))
   `(magit-branch-remote              ((,class (:foreground ,olive))))
   `(magit-diff-added                 ((,class (:background ,diff-green :foreground ,olive :extend t))))
   `(magit-diff-added-highlight       ((,class (:background ,diff-green :foreground ,olive :extend t))))
   `(magit-diff-removed               ((,class (:background ,diff-red :foreground ,red :extend t))))
   `(magit-diff-removed-highlight     ((,class (:background ,diff-red :foreground ,red :extend t))))
   `(magit-diff-context-highlight     ((,class (:background ,hl-line :extend t))))
   `(magit-diff-hunk-heading          ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(magit-diff-hunk-heading-highlight ((,class (:background ,bg-region :foreground ,fg :extend t))))
   `(magit-hash                       ((,class (:foreground ,fg-dim))))
   `(magit-log-author                 ((,class (:foreground ,red-dim))))
   `(magit-log-date                   ((,class (:foreground ,fg-dim))))

   ;; --- Diff ---
   `(diff-added                       ((,class (:background ,diff-green :foreground ,olive :extend t))))
   `(diff-removed                     ((,class (:background ,diff-red :foreground ,red :extend t))))
   `(diff-header                      ((,class (:background ,bg-alt :foreground ,fg :extend t))))
   `(diff-file-header                 ((,class (:background ,bg-hl :foreground ,fg :bold t :extend t))))
   `(diff-hunk-header                 ((,class (:background ,bg-hl :foreground ,fg-dim :extend t))))
   `(diff-refine-added                ((,class (:background "#C0E8C0" :foreground ,olive :bold t))))
   `(diff-refine-removed              ((,class (:background "#F0C0C0" :foreground ,red :bold t))))

   ;; --- Org mode ---
   `(org-level-1                      ((,class (:foreground ,fg :bold t :height 1.15))))
   `(org-level-2                      ((,class (:foreground ,red-dim :bold t :height 1.08))))
   `(org-level-3                      ((,class (:foreground ,navy :bold t))))
   `(org-level-4                      ((,class (:foreground ,brown))))
   `(org-level-5                      ((,class (:foreground ,olive))))
   `(org-level-6                      ((,class (:foreground ,fg-dim))))
   `(org-document-title               ((,class (:foreground ,fg :bold t :height 1.2))))
   `(org-document-info                ((,class (:foreground ,fg-dim))))
   `(org-todo                         ((,class (:foreground ,red :bold t))))
   `(org-done                         ((,class (:foreground ,olive :bold t))))
   `(org-date                         ((,class (:foreground ,fg-dim :underline t))))
   `(org-code                         ((,class (:foreground ,brown))))
   `(org-verbatim                     ((,class (:foreground ,olive))))
   `(org-block                        ((,class (:background ,bg-alt :extend t))))
   `(org-block-begin-line             ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-block-end-line               ((,class (:foreground ,fg-faint :background ,bg-alt :extend t))))
   `(org-table                        ((,class (:foreground ,fg))))
   `(org-link                         ((,class (:foreground ,red-dim :underline t))))

   ;; --- Markdown ---
   `(markdown-header-face-1           ((,class (:foreground ,fg :bold t :height 1.15))))
   `(markdown-header-face-2           ((,class (:foreground ,red-dim :bold t :height 1.08))))
   `(markdown-header-face-3           ((,class (:foreground ,navy :bold t))))
   `(markdown-code-face               ((,class (:foreground ,brown :background ,bg-alt))))
   `(markdown-inline-code-face        ((,class (:foreground ,brown :background ,bg-alt))))
   `(markdown-link-face               ((,class (:foreground ,red-dim))))
   `(markdown-url-face                ((,class (:foreground ,fg-dim))))

   ;; --- Eglot / Flymake ---
   `(eglot-highlight-symbol-face      ((,class (:background ,bg-hl :bold t))))
   `(flymake-error                    ((,class (:underline (:style wave :color ,red)))))
   `(flymake-warning                  ((,class (:underline (:style wave :color ,brown)))))
   `(flymake-note                     ((,class (:underline (:style wave :color ,olive)))))

   ;; --- Compilation ---
   `(compilation-error                ((,class (:foreground ,red :bold t))))
   `(compilation-warning              ((,class (:foreground ,brown))))
   `(compilation-info                 ((,class (:foreground ,olive))))

   ;; --- Dired ---
   `(dired-directory                  ((,class (:foreground ,fg :bold t))))
   `(dired-symlink                    ((,class (:foreground ,plum))))
   `(dired-ignored                    ((,class (:foreground ,fg-faint))))

   ;; --- Completions ---
   `(completions-common-part          ((,class (:foreground ,red :bold t))))
   `(completions-first-difference     ((,class (:foreground ,fg :bold t))))

   ;; --- Whitespace ---
   `(whitespace-trailing              ((,class (:background ,diff-red))))
   `(whitespace-tab                   ((,class (:foreground ,fg-faint))))
   `(whitespace-space                 ((,class (:foreground ,fg-faint))))

   ;; --- vterm ---
   `(vterm-color-black                ((,class (:foreground ,fg :background ,fg))))
   `(vterm-color-red                  ((,class (:foreground ,red :background ,red))))
   `(vterm-color-green                ((,class (:foreground ,olive :background ,olive))))
   `(vterm-color-yellow               ((,class (:foreground ,brown :background ,brown))))
   `(vterm-color-blue                 ((,class (:foreground ,navy :background ,navy))))
   `(vterm-color-magenta              ((,class (:foreground ,plum :background ,plum))))
   `(vterm-color-cyan                 ((,class (:foreground ,navy :background ,navy))))
   `(vterm-color-white                ((,class (:foreground ,bg-alt :background ,bg-alt)))))

  (custom-theme-set-variables
   'paper
   `(ansi-color-names-vector
     [,fg ,red ,olive ,brown ,navy ,plum ,navy ,bg-alt])))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-directory load-file-name)))

(provide-theme 'paper)
;;; paper-theme.el ends here
