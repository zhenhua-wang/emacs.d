;; -*- lexical-binding: t -*-

;;; code

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
     `(org-level-4 ((t (,@headline ,@variable-tuple :height 1.1 :italic t :slant italic))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :height 1.25 :foreground "#d08770"))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :height 1.50 :foreground "#446A46"))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground "#5e81ac"))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 2.0 :underline t)))))))

(custom-theme-set-faces
 'user
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch) :foreground "#242F9B" :background "#E8F9FD"))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (bold fixed-pitch) :weight bold))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch) :foreground "#B25068" :background "#FFEDDB"))))
 '(org-latex-and-related ((t (:inherit (shadow fixed-pitch))))))
 ;; hight code blocks
(set-face-attribute 'org-block-begin-line nil
		    :foreground "#4c566a"
		    :bold t
		    :italic t
		    :underline t
		    :overline nil
		    :extend t)
(set-face-attribute 'org-block-end-line nil
		    :background	(face-background 'org-block)
		    :bold t
		    :italic t
		    :underline nil
		    :overline nil
		    :extend t)

(provide 'zw-org-theme)
;;; zw-org-theme.el ends here
