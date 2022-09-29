;; -*- lexical-binding: t -*-

;;; code

(defun zw/set-md-headline ()
  (let* (
	 (variable-tuple (list ':font zw/font-title))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(markdown-header-face-6 ((t (,@headline ,@variable-tuple))))
     `(markdown-header-face-5 ((t (,@headline ,@variable-tuple))))
     `(markdown-header-face-4 ((t (,@headline ,@variable-tuple :foreground ,(face-foreground 'default)))))
     `(markdown-header-face-3 ((t (,@headline ,@variable-tuple :height 1.25 :foreground ,(face-foreground 'default)))))
     `(markdown-header-face-2 ((t (,@headline ,@variable-tuple :height 1.5 :foreground ,(face-foreground 'default)))))
     `(markdown-header-face-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground ,(face-foreground 'default)))))
     `(markdown-metadata-value-face ((t (,@variable-tuple :inherit markdown-metadata-value-face :height 1.7 :underline t :weight bold :foreground ,base-font-color))))
     `(markdown-metadata-key-face ((t (:inherit (thin markdown-metadata-key-face) :height 0.8 :foreground "dark orange"))))))

  (set-face-attribute 'markdown-header-delimiter-face nil :inherit '(font-lock-comment-face fixed-pitch) :height 0.8)
  (set-face-attribute 'markdown-language-info-face nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'markdown-code-face nil :inherit 'fixed-pitch :extend t))

(provide 'zw-md-theme)
;;; zw-md-theme.el ends here
