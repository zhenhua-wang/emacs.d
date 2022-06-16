;; Replace list hyphen with dot
(font-lock-add-keywords 'org-mode
                        '(("^ *\\([-]\\) "
                           (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

(defun zw/set-org-headline ()
  (let* (
	 (variable-tuple (list ':font zw/font-title))
	 (base-font-color     (face-foreground 'default nil 'default))
	 (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline ,@variable-tuple))))
     `(org-level-7 ((t (,@headline ,@variable-tuple))))
     `(org-level-6 ((t (,@headline ,@variable-tuple))))
     `(org-level-5 ((t (,@headline ,@variable-tuple))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1 :italic t :foreground "#FF8C94" :slant italic))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25 :foreground "#D08770"))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.50 :foreground "#88C0D0"))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground "#5E81AC"))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline t)))))))

(custom-theme-set-faces
   'user
   '(org-block ((t (:inherit fixed-pitch))))
   '(org-code ((t (:inherit (shadow fixed-pitch)))))
   '(org-document-info ((t (:foreground "dark orange"))))
   '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
   '(org-link ((t (:foreground "royal blue" :underline t))))
   '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-property-value ((t (:inherit fixed-pitch))) t)
   '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
   '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
   ;; '(org-latex-and-related ((t (:foreground "#EBCB8B"))))
   ;; hight code blocks
   '(org-block-begin-line ((t (:background "#4C566A" :foreground "#bfbfbf"
                                           :bold t :height 1.0))))
   '(org-block-end-line ((t (:background "#4C566A" :foreground "#bfbfbf"
                                         :bold t :height 1.0)))))

(provide 'org-theme)
