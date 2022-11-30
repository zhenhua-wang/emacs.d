;; -*- lexical-binding: t -*-

(defun zw/set-theme (block-background modeline-hightlight modeline-text-on-hightlight modeline-3d)
  (let* ((base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color))
         (fixed-tuple        `(:font "Hack"))
         (variable-tuple     `(:font "EB Garamond")))

    (custom-theme-set-faces
     'user
     ;; default
     `(default ((t (:font "Roboto Mono" :height 150))))
     `(fixed-pitch ((t (,@fixed-tuple :weight normal :height 150))))
     `(variable-pitch ((t (,@variable-tuple :weight light :height 200))))

     ;; org
     `(org-level-8 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-7 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-6 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-5 ((t (,@headline ,@variable-tuple :weight SemiBold))))
     `(org-level-4 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25))))
     `(org-level-3 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25))))
     `(org-level-2 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.25))))
     `(org-level-1 ((t (,@headline ,@variable-tuple :weight SemiBold :height 1.5))))
     `(org-document-title ((t (,@headline ,@variable-tuple :height 1.625 :underline t))))
     ;; setup fixed pitch fonts
     `(org-ellipsis ((t (:inherit fixed-pitch))))
     `(org-meta-line ((t (:inherit (shadow fixed-pitch)))))
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
     `(org-block-begin-line ((t (:inherit 'fixed-pitch :background ,block-background :bold t :italic t :underline t :extend t))))
     `(org-block-end-line ((t (:background ,block-background :bold t :italic t :extend t))))
     `(org-code ((t (:inherit (shadow fixed-pitch) :background ,block-background))))
     `(org-verbatim ((t (:inherit (shadow fixed-pitch) :background ,block-background :box ,(face-foreground 'default)))))
     ;; org-visual-indent
     `(org-visual-indent-pipe-face ((t (:foreground ,(face-foreground 'default) :background ,(face-foreground 'default) :height .1))))
     `(org-visual-indent-blank-pipe-face ((t (:foreground ,(face-background 'default) :background ,(face-background 'default) :height .1))))

     ;; markdown
     `(markdown-header-face-6 ((t (,@headline ,@variable-tuple))))
     `(markdown-header-face-5 ((t (,@headline ,@variable-tuple))))
     `(markdown-header-face-4 ((t (,@headline ,@variable-tuple))))
     `(markdown-header-face-3 ((t (,@headline ,@variable-tuple :height 1.25))))
     `(markdown-header-face-2 ((t (,@headline ,@variable-tuple :height 1.5))))
     `(markdown-header-face-1 ((t (,@headline ,@variable-tuple :height 1.75))))
     `(markdown-metadata-value-face ((t (,@variable-tuple :foreground ,base-font-color :height 2.0 :underline t :bold t))))
     `(markdown-metadata-key-face ((t (:inherit (thin fixed-pitch) :height 0.8))))
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
     `(mode-line ((t (:foreground ,(face-foreground 'default) :box ,(when modeline-3d '(:line-width 1 :style released-button))))))
     `(mode-line-inactive ((t (:foreground ,(face-foreground 'font-lock-comment-face)))))
     `(mode-line-highlight ((t (:foreground ,modeline-text-on-hightlight :background ,modeline-hightlight))))
     `(zw/modeline-default-active ((t (:foreground ,(face-foreground 'mode-line)))))
     `(zw/modeline-default-inactive ((t (:foreground ,(face-foreground 'font-lock-comment-face)))))
     `(zw/modeline-highlight-foreground-active ((t (:foreground ,modeline-hightlight))))
     `(zw/modeline-modified-active ((t (:inherit warning))))

     ;; tab-bar
     `(tab-bar ((t (:foreground ,(face-foreground 'default)))))

     ;; show paren
     `(show-paren-match ((t (:background ,(face-foreground 'warning) :foreground "black" :weight extra-bold))))

     ;; hl-line
     `(hl-line ((t (:foreground ,(face-foreground 'default) :background ,(doom-darken (face-background 'default) 0.15)))))

     ;; diff-hl
     `(diff-hl-change ((t (:foreground "black" :background "#FF9F29"))))
     `(diff-hl-insert ((t (:foreground "black" :background "#3CCF4E"))))
     `(diff-hl-delete ((t (:foreground "white" :background "#E94560"))))

     ;; diredfl
     `(diredfl-dir-name ((t (:bold t))))

     ;; company-mode
     `(company-tooltip ((t (:inherit tooltip ,@fixed-tuple))))
     `(company-tooltip-selection ((t (:weight bold))))
     `(company-tooltip-annotation ((t (:slant normal))))
     `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :slant normal :weight bold))))
     `(company-posframe-active-backend-name ((t (:inherit company-tooltip :background unspecified :weight bold))))
     `(company-posframe-inactive-backend-name ((t (:inherit company-tooltip :background unspecified)))))))

(pcase (frame-parameter nil 'background-mode)
  ('light (zw/set-theme (doom-darken (face-background 'default) 0.06) "#0000c0" "white" t))
  ('dark (zw/set-theme (doom-lighten (face-background 'default) 0.06) "#51afef" "black" nil)))

(provide 'zw-theme)
