;;; theme.el --- Theme manipulation helpers  -*- lexical-binding: t; -*-

(defun desaturate-color (color-hex)
  "Convert COLOR-HEX to its desaturated equivalent."
  (require 'color)
  (apply 'color-rgb-to-hex
         (append (apply 'color-hsl-to-rgb
                        (apply 'color-desaturate-hsl
                               `(,@(apply 'color-rgb-to-hsl
                                          (color-name-to-rgb color-hex)) 100)))
                 '(2))))

(defun transform-theme-colors (fn)
  "Apply FN to colors on every active face.
FN receives face symbol and current color, returns new color."
  (mapc
   (lambda (face)
     (mapc
      (lambda (attr)
        (let ((current (face-attribute face attr)))
          (unless (or (not current) (listp current)
                      (string= current "unspecified") (string= current "t"))
            (set-face-attribute face nil attr (funcall fn face current)))))
      '(:foreground :background :underline :overline :box :strike-through :distant-foreground))
     (mapc
      (lambda (complex-attr)
        (let* ((full (copy-tree (face-attribute face complex-attr)))
               (current (if (listp full) (member :color full))))
          (unless (or (not current) (not (listp full)))
            (setcar (cdr current) (funcall fn face (cadr current)))
            (set-face-attribute face nil complex-attr full))))
      '(:underline :overline :box)))
   (face-list)))

;;;###autoload
(defun desaturate-theme ()
  "Desaturate all currently active face colors."
  (interactive)
  (transform-theme-colors (lambda (_face color) (desaturate-color color))))

;;;###autoload
(defun invert-theme ()
  "Invert all currently active colors to their complement."
  (interactive)
  (require 'color)
  (transform-theme-colors
   (lambda (_face color) (apply 'color-rgb-to-hex (color-complement color))))
  (let ((current-ns-appearance (assoc 'ns-appearance default-frame-alist)))
    (cond ((eq (cdr current-ns-appearance) 'light)
           (setf (cdr current-ns-appearance) 'dark))
          ((eq (cdr current-ns-appearance) 'dark)
           (setf (cdr current-ns-appearance) 'light)))))

(provide 'theme)
;;; theme.el ends here
