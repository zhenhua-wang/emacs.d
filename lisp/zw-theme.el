;; -*- lexical-binding: t -*-

(let* ((base-font-color     (face-foreground 'default nil 'default))
       (headline           `(:inherit default :weight bold :foreground ,base-font-color))
       (block-background    "#EEEEEE")
       (variable-tuple      (list ':font zw/font-title)))

  (custom-theme-set-faces
   'user
   ;; org
   `(org-level-8 ((t (,@headline ,@variable-tuple :weight SemiBold))))
   `(org-level-7 ((t (,@headline ,@variable-tuple :weight SemiBold))))
   `(org-level-6 ((t (,@headline ,@variable-tuple :weight SemiBold))))
   `(org-level-5 ((t (,@headline ,@variable-tuple :weight SemiBold))))
   `(org-level-4 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25 :foreground "#FF7777"))))
   `(org-level-3 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25 :foreground "#d08770"))))
   `(org-level-2 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25 :foreground "#446A46"))))
   `(org-level-1 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.5 :foreground "#5e81ac"))))
   `(org-document-title ((t (,@headline ,@variable-tuple :height 1.625 :underline t))))
   ;; setup fixed pitch fonts
   `(org-ellipsis ((t (:inherit fixed-pitch))))
   `(org-meta-line ((t (:inherit (bold fixed-pitch)))))
   `(org-table ((t (:inherit fixed-pitch))))
   `(org-formula ((t (:inherit fixed-pitch))))
   `(org-latex-and-related ((t (:inherit (shadow fixed-pitch)))))
   `(org-link ((t (:inherit fixed-pitch :underline t))))
   `(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(org-checkbox ((t (:inherit 'fixed-pitch))))
   `(org-property-value ((t (:inherit fixed-pitch))))
   `(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
   `(org-document-info ((t (:foreground "dark orange"))))
   `(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
   `(org-block ((t (:inherit fixed-pitch :background ,block-background))))
   `(org-block-begin-line ((t (:background ,block-background :bold t :italic t :underline t :extend t))))
   `(org-block-end-line ((t (:background ,block-background :bold t :italic t :extend t))))
   `(org-code ((t (:inherit (shadow fixed-pitch) :background ,block-background :box ,(face-foreground 'default)))))
   `(org-verbatim ((t (:inherit (shadow fixed-pitch) :background ,block-background :box ,(face-foreground 'default)))))

   ;; markdown
   `(markdown-header-face-6 ((t (,@headline ,@variable-tuple))))
   `(markdown-header-face-5 ((t (,@headline ,@variable-tuple))))
   `(markdown-header-face-4 ((t (,@headline ,@variable-tuple :foreground ,(face-foreground 'default)))))
   `(markdown-header-face-3 ((t (,@headline ,@variable-tuple :height 1.25 :foreground ,(face-foreground 'default)))))
   `(markdown-header-face-2 ((t (,@headline ,@variable-tuple :height 1.5 :foreground ,(face-foreground 'default)))))
   `(markdown-header-face-1 ((t (,@headline ,@variable-tuple :height 1.75 :foreground ,(face-foreground 'default)))))
   `(markdown-metadata-value-face ((t (,@variable-tuple :inherit markdown-metadata-value-face :height 1.7 :underline t :weight bold :foreground ,base-font-color))))
   `(markdown-metadata-key-face ((t (:inherit (thin markdown-metadata-key-face) :height 0.8 :foreground "dark orange"))))
   `(markdown-header-delimiter-face ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
   `(markdown-language-info-face ((t (:inherit (font-lock-comment-face fixed-pitch)))))
   `(markdown-code-face ((t (:inherit fixed-pitch :background ,block-background :extend t))))

   ;; vterm
   `(vterm-color-black ((t (:foreground "#3B4252" :background "#3B4252"))))
   `(vterm-color-red ((t (:foreground "#B25068" :background "#B25068"))))
   `(vterm-color-green ((t (:foreground "#2EB086" :background "#2EB086"))))
   `(vterm-color-yellow ((t (:foreground "#F4E06D" :background "#F4E06D"))))
   `(vterm-color-blue ((t (:foreground "#47B5FF" :background "#47B5FF"))))
   `(vterm-color-magenta ((t (:foreground "#FF869E" :background "#FF869E"))))
   `(vterm-color-cyan ((t (:foreground "#4CACBC" :background "#4CACBC"))))
   `(vterm-color-white ((t (:foreground "#E5E9F0" :background "#E5E9F0"))))

   ;; vc
   '(vc-edited-state ((t (:foreground "#FF9F29"))))
   '(vc-locally-added-state ((t (:foreground "#3CCF4E"))))
   '(vc-removed-state ((t (:foreground "#E94560"))))

   ;; modeline
   `(mode-line ((t (:foreground ,(face-foreground 'default) :box (:line-width 1 :style released-button)))))
   `(mode-linee-inactive ((t (:foreground ,(face-foreground 'font-lock-comment-face)))))
   `(zw/modeline-major-mode-active ((t (:foreground "#0000c0" :bold t))))
   `(zw/modeline-tab-index-active ((t (:foreground "#0000c0"))))))

(provide 'zw-theme)
