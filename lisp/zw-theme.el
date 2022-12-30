;; -*- lexical-binding: t -*-

(defun zw/set-theme (theme-params)
  (let* ((base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color))
         (fixed-font        `(:font "Hack"))
         (variable-font     `(:font "EB Garamond"))
         (block-bg (alist-get 'block-bg theme-params))
         (modeline-highlight-bg (alist-get 'modeline-highlight-bg theme-params))
         (modeline-highlight-fg (alist-get 'modeline-highlight-fg theme-params))
         (modeline-3d-p (alist-get 'modeline-3d-p theme-params))
         (region (alist-get 'region theme-params)))
    ;; default fonts
    (let ((default-font (font-spec :name "Noto Sans Mono" :size 15.0))
          (cn-font (font-spec :name "Noto Sans Mono CJK SC" :size 13.0)))
      (set-face-attribute 'default nil :font default-font)
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font t charset cn-font)))

    (custom-theme-set-faces
     'user
     ;; fonts
     `(fixed-pitch ((t (,@fixed-font :height 150))))
     `(variable-pitch ((t (,@variable-font :height 200))))

     ;; org
     `(org-level-8 ((t (,@headline ,@variable-font :weight SemiBold))))
     `(org-level-7 ((t (,@headline ,@variable-font :weight SemiBold))))
     `(org-level-6 ((t (,@headline ,@variable-font :weight SemiBold))))
     `(org-level-5 ((t (,@headline ,@variable-font :weight SemiBold))))
     `(org-level-4 ((t (,@headline ,@variable-font :weight SemiBold :height 1.5))))
     `(org-level-3 ((t (,@headline ,@variable-font :weight SemiBold :height 1.5))))
     `(org-level-2 ((t (,@headline ,@variable-font :weight SemiBold :height 1.5))))
     `(org-level-1 ((t (,@headline ,@variable-font :weight SemiBold :height 1.75))))
     `(org-document-title ((t (,@headline ,@variable-font :height 2.0 :underline t))))
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
     `(org-block ((t (:inherit fixed-pitch :background ,block-bg))))
     `(org-block-begin-line ((t (:inherit 'fixed-pitch :background ,block-bg :bold t :italic t :underline t :extend t))))
     `(org-block-end-line ((t (:background ,block-bg :bold t :italic t :extend t))))
     `(org-code ((t (:inherit (shadow fixed-pitch) :background ,block-bg))))
     `(org-verbatim ((t (:inherit (shadow fixed-pitch) :background ,block-bg :box ,(face-foreground 'default)))))
     ;; org-visual-indent
     `(org-visual-indent-pipe-face ((t (:foreground ,(face-foreground 'default) :background ,(face-foreground 'default) :height .1))))
     `(org-visual-indent-blank-pipe-face ((t (:foreground ,(face-background 'default) :background ,(face-background 'default) :height .1))))

     ;; markdown
     `(markdown-header-face-6 ((t (,@headline ,@variable-font))))
     `(markdown-header-face-5 ((t (,@headline ,@variable-font))))
     `(markdown-header-face-4 ((t (,@headline ,@variable-font :height 1.5))))
     `(markdown-header-face-3 ((t (,@headline ,@variable-font :height 1.5))))
     `(markdown-header-face-2 ((t (,@headline ,@variable-font :height 1.5))))
     `(markdown-header-face-1 ((t (,@headline ,@variable-font :height 1.75))))
     `(markdown-metadata-value-face ((t (,@variable-font :foreground ,base-font-color :height 2.0 :underline t :bold t))))
     `(markdown-metadata-key-face ((t (:inherit (thin fixed-pitch) :height 0.8))))
     `(markdown-header-delimiter-face ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
     `(markdown-language-info-face ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     `(markdown-code-face ((t (:inherit fixed-pitch :background ,block-bg :extend t))))
     `(markdown-markup-face ((t (:inherit (font-lock-comment-face fixed-pitch) :foreground unspecified :slant normal))))

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
     `(mode-line ((t (:foreground ,(face-foreground 'default) :box ,(when modeline-3d-p '(:line-width 1 :style released-button))))))
     `(mode-line-inactive ((t (:foreground ,(face-foreground 'font-lock-comment-face)))))
     `(mode-line-highlight ((t (:foreground ,modeline-highlight-fg :background ,modeline-highlight-bg))))
     `(zw/modeline-default-active ((t (:foreground ,(face-foreground 'mode-line)))))
     `(zw/modeline-default-inactive ((t (:foreground ,(face-foreground 'font-lock-comment-face)))))
     `(zw/modeline-highlight-foreground-active ((t (:foreground ,modeline-highlight-bg))))
     `(zw/modeline-modified-active ((t (:inherit warning))))

     ;; tab-bar
     `(tab-bar ((t (:foreground ,(face-foreground 'default) :weight SemiBold))))
     `(zw/tab-bar-default-selected ((t (:inherit tab-bar :background ,(face-background 'tab-bar)))))
     `(zw/tab-bar-menu-bar ((t (:inherit zw/tab-bar-default-selected :bold t))))
     `(zw/tab-bar-tab-path-selected ((t (:inherit zw/tab-bar-default-selected :bold t :foreground ,modeline-highlight-bg))))
     `(zw/tab-bar-tab-battery-load-default ((t (:inherit zw/tab-bar-default-selected))))
     `(zw/tab-bar-tab-battery-load-charging ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'success)))))
     `(zw/tab-bar-tab-battery-load-low ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'warning)))))
     `(zw/tab-bar-tab-battery-load-critical ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'error)))))

     ;; show paren
     `(show-paren-match ((t (:background ,(face-foreground 'warning) :foreground "black" :weight extra-bold))))

     ;; region
     `(region ((t (:background ,region))))

     ;; diff-hl
     `(diff-hl-change ((t (:foreground "black" :background "#FF9F29"))))
     `(diff-hl-insert ((t (:foreground "black" :background "#3CCF4E"))))
     `(diff-hl-delete ((t (:foreground "white" :background "#E94560"))))

     ;; diredfl
     `(diredfl-dir-name ((t (:bold t))))

     ;; outline minor
     `(outline-minor-1 ((t (:inherit (outline-minor-0 outline-1) :overline t))))

     ;; company-mode
     `(company-tooltip ((t (:inherit tooltip ,@fixed-font :height 150))))
     `(company-tooltip-selection ((t (:weight bold))))
     `(company-tooltip-annotation ((t (:slant normal))))
     `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :slant normal :weight bold))))
     `(company-posframe-active-backend-name ((t (:inherit company-tooltip :background unspecified :weight bold))))
     `(company-posframe-inactive-backend-name ((t (:inherit company-tooltip :background unspecified)))))))

;; set theme
(let ((light-theme-params `((block-bg . ,(doom-darken (face-background 'default) 0.06))
                            (modeline-highlight-bg . "#0000c0")
                            (modeline-highlight-fg . "white")
                            (modeline-3d-p . t)
                            (region . ,(doom-darken (face-background 'default) 0.2))))
      (dark-theme-params `((block-bg . ,(doom-lighten (face-background 'default) 0.06))
                           (modeline-highlight-bg . "#51afef")
                           (modeline-highlight-fg . "black")
                           (modeline-3d-p . nil)
                           (region . ,(doom-lighten (face-background 'default) 0.2)))))
  (pcase (frame-parameter nil 'background-mode)
    ('light (zw/set-theme light-theme-params))
    ('dark (zw/set-theme dark-theme-params))))

(provide 'zw-theme)
