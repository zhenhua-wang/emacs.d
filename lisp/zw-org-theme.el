;; -*- lexical-binding: t -*-

;;; code

(defun zw/set-org-headline ()
  (let* (
	 (variable-tuple (list ':font zw/font-title))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-7 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-6 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-5 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25 :foreground "#E94560"))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25 :foreground "#d08770"))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25 :foreground "#446A46"))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.5 :foreground "#5e81ac"))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.625 :underline t))))))

  ;; setup fixed pitch fonts
  (set-face-attribute 'org-block nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-meta-line nil :inherit '(bold fixed-pitch))
  (set-face-attribute 'org-code nil :inherit '(shadow fixed-pitch) :foreground "#0078AA" :background (face-background 'org-block) :box (face-foreground 'default))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch) :foreground "#B25068" :background (face-background 'org-block) :box (face-foreground 'default))
  (set-face-attribute 'org-table nil :inherit 'fixed-pitch :foreground "#83a598")
  (set-face-attribute 'org-formula nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-latex-and-related nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-link nil :inherit 'fixed-pitch :foreground "royal blue" :underline t)
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-property-value nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-tag nil :inherit '(shadow fixed-pitch) :weight 'bold :height 0.8)
  (set-face-attribute 'org-document-info nil :foreground "dark orange")
  (set-face-attribute 'org-document-info-keyword nil :inherit '(shadow fixed-pitch))

  ;; hight code blocks
  (set-face-attribute 'org-block-begin-line nil
		      :foreground "#4c566a"
		      :bold t
		      :italic t
		      :underline t
		      :overline nil
		      :extend t)
  (set-face-attribute 'org-block-end-line nil
		      :background (face-background 'org-block)
		      :bold t
		      :italic t
		      :underline nil
		      :overline nil
		      :extend t))

(provide 'zw-org-theme)
;;; zw-org-theme.el ends here
