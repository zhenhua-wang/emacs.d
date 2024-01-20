;; -*- lexical-binding: t -*-

;; * doom theme
(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil)
  (doom-themes-visual-bell-config))

;; * zw theme
(defun zw/theme--set-theme (theme-params)
  (let* ((base-font-color         (face-foreground 'default nil 'default))
         (fixed-font             `(:font "JetBrains Mono"))
         (fixed-font-height       150)
         (variable-font          `(:font "EB Garamond"))
         (variable-font-height    200)
         (modeline-height         130)
         (tab-bar-height          120)
         (block-bg (alist-get 'block-bg theme-params))
         (modeline-highlight-bg (alist-get 'modeline-highlight-bg theme-params))
         (modeline-highlight-fg (alist-get 'modeline-highlight-fg theme-params))
         (modeline-highlight-inactive-bg (alist-get 'modeline-highlight-inactive-bg theme-params))
         (modeline-3d-p (alist-get 'modeline-3d-p theme-params))
         (tab-bar-box (alist-get 'tab-bar-box theme-params))
         (region (alist-get 'region theme-params)))

    ;; default fonts
    (let ((default-font (font-spec :name "Noto Sans Mono" :size 15.0))
          (cn-font (font-spec :name "Noto Sans Mono CJK SC" :size 11.0))
          (emoji-font (font-spec :name "Noto Color Emoji" :size 11.0)))
      (set-face-attribute 'default nil :font default-font)
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font t charset cn-font))
      (set-fontset-font t 'symbol emoji-font))

    (custom-theme-set-faces
     'user
     ;; fonts
     `(fixed-pitch ((t (,@fixed-font :height ,fixed-font-height))))
     `(variable-pitch ((t (,@variable-font :height ,variable-font-height))))

     ;; child frame
     `(child-frame-border ((t (:background ,(face-background 'highlight)))))

     ;; org
     `(org-level-8 ((t (:inherit outline-8 ,@variable-font :weight SemiBold))))
     `(org-level-7 ((t (:inherit outline-7 ,@variable-font :weight SemiBold))))
     `(org-level-6 ((t (:inherit outline-6 ,@variable-font :weight SemiBold))))
     `(org-level-5 ((t (:inherit outline-5 ,@variable-font :weight SemiBold))))
     `(org-level-4 ((t (:inherit outline-4 ,@variable-font :weight SemiBold :height 1.25))))
     `(org-level-3 ((t (:inherit outline-3 ,@variable-font :weight SemiBold :height 1.25))))
     `(org-level-2 ((t (:inherit outline-2 ,@variable-font :weight SemiBold :height 1.25))))
     `(org-level-1 ((t (:inherit outline-1 ,@variable-font :weight SemiBold :height 1.5))))
     `(org-document-title ((t (,@variable-font :foreground ,base-font-color :weight Bold :height 1.7 :underline t))))
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

     ;; markdown
     `(markdown-header-face-6 ((t (:inherit outline-6 ,@variable-font :weight SemiBold))))
     `(markdown-header-face-5 ((t (:inherit outline-5 ,@variable-font :weight SemiBold))))
     `(markdown-header-face-4 ((t (:inherit outline-4 ,@variable-font :weight SemiBold :height 1.25))))
     `(markdown-header-face-3 ((t (:inherit outline-3 ,@variable-font :weight SemiBold :height 1.25))))
     `(markdown-header-face-2 ((t (:inherit outline-2 ,@variable-font :weight SemiBold :height 1.25))))
     `(markdown-header-face-1 ((t (:inherit outline-1 ,@variable-font :weight SemiBold :height 1.5))))
     `(markdown-metadata-value-face ((t (,@variable-font :foreground ,base-font-color :weight Bold :height 1.7 :underline t))))
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
     `(mode-line ((t (:height ,modeline-height :box ,(when modeline-3d-p '(:line-width 1 :style released-button))))))
     `(mode-line-inactive ((t (:inherit mode-line :foreground ,(face-foreground 'font-lock-comment-face)))))
     `(mode-line-highlight ((t (:inherit mode-line :foreground ,modeline-highlight-fg :background ,modeline-highlight-bg))))
     `(zw/modeline-default-active ((t (:height ,modeline-height :foreground ,(face-foreground 'mode-line)))))
     `(zw/modeline-default-inactive ((t (:height ,modeline-height :foreground ,(face-foreground 'font-lock-comment-face)))))
     `(zw/modeline-modified-active ((t (:inherit (warning zw/modeline-buffer-name-active)))))
     `(zw/modeline-highlight-background-inactive ((t (:inherit zw/modeline-default-inactive :background ,modeline-highlight-inactive-bg))))

     ;; tab-bar
     `(tab-bar ((t (:foreground ,(face-foreground 'default) :height ,tab-bar-height :weight regular :box ,tab-bar-box))))
     `(zw/tab-bar-default-selected ((t (:inherit tab-bar))))
     `(zw/tab-bar-menu-bar ((t (:inherit zw/tab-bar-default-selected :bold t))))
     `(zw/tab-bar-tab-path-selected ((t (:inherit zw/tab-bar-default-selected :bold t :foreground ,modeline-highlight-bg))))
     `(zw/tab-bar-tab-battery-load-default ((t (:inherit zw/tab-bar-default-selected :bold t))))
     `(zw/tab-bar-tab-battery-load-charging ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'success) :bold t))))
     `(zw/tab-bar-tab-battery-load-low ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'warning) :bold t))))
     `(zw/tab-bar-tab-battery-load-critical ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'error) :bold t))))

     ;; key-cast
     `(keycast-key ((t (:height ,tab-bar-height))))
     `(keycast-command ((t (:height ,tab-bar-height))))

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
     `(company-tooltip ((t (:inherit tooltip ,@fixed-font :height ,fixed-font-height))))
     `(company-tooltip-selection ((t (:weight bold))))
     `(company-tooltip-annotation ((t (:slant normal))))
     `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :slant normal :weight bold))))
     `(company-posframe-active-backend-name ((t (:inherit company-tooltip :background unspecified :weight bold))))
     `(company-posframe-inactive-backend-name ((t (:inherit company-tooltip :background unspecified)))))))

(defun zw/theme-set-theme ()
  (let ((light-theme-params `((block-bg . ,(doom-darken (face-background 'default) 0.06))
                              ;; (modeline-highlight-bg . "#0000c0")
                              ;; (modeline-highlight-fg . "#ffffff")
                              (modeline-highlight-bg . ,(face-background 'highlight))
                              (modeline-highlight-fg . ,(face-foreground 'highlight))
                              (modeline-highlight-inactive-bg . ,(doom-darken (face-background 'mode-line-inactive) 0.05))
                              (modeline-3d-p . nil)
                              (tab-bar-box . ,(doom-darken (face-background 'tab-bar) 0.05))
                              (region . ,(doom-darken (face-background 'default) 0.2))))
        (dark-theme-params `((block-bg . ,(doom-lighten (face-background 'default) 0.06))
                             (modeline-highlight-bg . ,(face-background 'highlight))
                             (modeline-highlight-fg . ,(face-foreground 'highlight))
                             (modeline-highlight-inactive-bg . ,(doom-lighten (face-background 'mode-line-inactive) 0.05))
                             (modeline-3d-p . nil)
                             (tab-bar-box . ,(doom-lighten (face-background 'tab-bar) 0.05))
                             (region . ,(doom-lighten (face-background 'default) 0.2)))))
    (pcase (frame-parameter nil 'background-mode)
      ('light (zw/theme--set-theme light-theme-params))
      ('dark (zw/theme--set-theme dark-theme-params)))))

;; * load theme
(setq zw/theme-selector
      (expand-file-name "zw-select-theme.el" user-emacs-directory))
(when (not (file-exists-p zw/theme-selector))
  (write-region "(load-theme 'doom-one t)" nil zw/theme-selector))
(load zw/theme-selector)

;; load custom faces
(zw/theme-set-theme)
(add-hook 'server-after-make-frame-hook 'zw/theme-set-theme)
(advice-add
 #'consult-theme
 :after (lambda (arg)
          (zw/theme-set-theme)
          (write-region (format "(load-theme '%s t)" (car custom-enabled-themes))
                        nil zw/theme-selector)))

;; * Provide
(provide 'zw-theme)
