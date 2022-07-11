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
     `(markdown-header-face-4 ((t (,@headline ,@variable-tuple :foreground "#FF8C94"))))
     `(markdown-header-face-3 ((t (,@headline ,@variable-tuple :height 1.25 :foreground "#D08770"))))
     `(markdown-header-face-2 ((t (,@headline ,@variable-tuple :height 1.5 :foreground "#446A46"))))
     `(markdown-header-face-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground "#5E81AC"))))
     `(markdown-metadata-value-face ((t (,@variable-tuple :inherit markdown-metadata-value-face :height 1.7 :underline t :weight bold :foreground ,base-font-color))))
     `(markdown-metadata-key-face ((t (:inherit (thin markdown-metadata-key-face) :height 0.8)))))))

(provide 'zw-md-theme)
;;; zw-md-theme.el ends here
