;; -*- lexical-binding: t -*-

;; * Font
(let ((default-font (font-spec :name "Noto Sans Mono" :size 15.0))
      (cn-font (font-spec :name "Noto Sans Mono CJK SC" :size 15.0))
      (emoji-font (font-spec :name "Noto Color Emoji" :size 15.0))
      (fixed-pitch-font (font-spec :name "JetBrains Mono"))
      (variable-pitch-font (font-spec :name "EB Garamond")))
  (when (find-font default-font)
    (set-face-attribute 'default nil :font default-font))
  (when (find-font cn-font)
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset cn-font)))
  (when (find-font emoji-font)
    (set-fontset-font t 'symbol emoji-font))
  (when (find-font fixed-pitch-font)
    (set-face-attribute 'fixed-pitch nil :font fixed-pitch-font))
  (when (find-font variable-pitch-font)
    (set-face-attribute 'variable-pitch nil :font variable-pitch-font)))

;; * Doom theme
(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil))

;; * ZW theme
(defun zw/theme--set-theme (theme-params)
  (let* ((base-font-color          (face-foreground 'default nil 'default))
         (mode-line-color          (face-background 'mode-line nil 'default))
         (tab-bar-color            (face-background 'tab-bar nil 'default))
         (shadow-color             (face-foreground 'shadow nil 'default))
         (block-color              (alist-get 'block-color theme-params))
         (highlight-color          (face-background 'highlight))
         (highlight-revert-color   (face-foreground 'highlight))
         (highlight-alt-color      (face-foreground 'warning nil 'default))
         (mode-line-inactive-color (alist-get 'mode-line-inactive-color theme-params))
         (tab-bar-box              (alist-get 'tab-bar-box theme-params))
         (modeline-height          130)
         (tab-bar-height           120))
    (custom-theme-set-faces
     'user
     ;; child frame
     `(child-frame-border ((t (:background ,highlight-color))))

     ;; modeline
     `(mode-line
       ((t (:height ,modeline-height))))
     `(mode-line-inactive
       ((t (:inherit mode-line :foreground ,shadow-color))))
     `(mode-line-highlight
       ((t (:inherit mode-line :foreground ,highlight-revert-color :background ,highlight-color))))
     `(zw/modeline-default-active
       ((t (:inherit mode-line :height ,modeline-height))))
     `(zw/modeline-default-inactive
       ((t (:inherit mode-line :height ,modeline-height :foreground ,shadow-color))))
     `(zw/modeline-modified-active
       ((t (:inherit zw/modeline-buffer-name-active :foreground ,highlight-alt-color))))
     `(zw/modeline-highlight-foreground-active
       ((t (:inherit mode-line :foreground ,highlight-color))))
     `(zw/modeline-highlight-background-inactive
       ((t (:inherit zw/modeline-default-inactive :background ,mode-line-inactive-color))))
     `(zw/modeline-separator-active
       ((t (:inherit mode-line :background ,mode-line-color))))

     ;; tab-bar
     `(tab-bar
       ((t (:foreground ,base-font-color :height ,tab-bar-height :weight regular :box ,tab-bar-box))))
     `(zw/tab-bar-default-selected
       ((t (:inherit tab-bar))))
     `(zw/tab-bar-menu-bar
       ((t (:inherit zw/tab-bar-default-selected :bold t))))
     `(zw/tab-bar-tab-path-selected
       ((t (:inherit zw/tab-bar-default-selected :bold t :foreground ,highlight-color))))
     `(zw/tab-bar-tab-battery-load-default
       ((t (:inherit zw/tab-bar-default-selected :bold t))))
     `(zw/tab-bar-tab-battery-load-charging
       ((t (:inherit (success zw/tab-bar-default-selected) :bold t))))
     `(zw/tab-bar-tab-battery-load-low
       ((t (:inherit (warning zw/tab-bar-default-selected) :bold t))))
     `(zw/tab-bar-tab-battery-load-critical
       ((t (:inherit (error zw/tab-bar-default-selected) :bold t))))

     ;; tab-line
     `(tab-line ((t (:background ,tab-bar-color :underline ,tab-bar-box))))

     ;; header-line
     `(header-line ((t (:background ,mode-line-color :underline ,tab-bar-box :bold t))))

     ;; vc
     '(vc-edited-state ((t (:foreground "#FF9F29"))))
     '(vc-locally-added-state ((t (:foreground "#3CCF4E"))))
     '(vc-removed-state ((t (:foreground "#E94560"))))

     ;; diff-hl
     `(diff-hl-change ((t (:foreground "black" :background "#FF9F29"))))
     `(diff-hl-insert ((t (:foreground "black" :background "#3CCF4E"))))
     `(diff-hl-delete ((t (:foreground "white" :background "#E94560"))))

     ;; key-cast
     `(keycast-key ((t (:height ,tab-bar-height))))
     `(keycast-command ((t (:height ,tab-bar-height))))

     ;; show paren
     `(show-paren-match ((t (:background ,highlight-alt-color :foreground "black" :weight extra-bold))))

     ;; diredfl
     `(diredfl-dir-name ((t (:bold t))))

     ;; outline minor
     `(outline-minor-1 ((t (:inherit (outline-minor-0 outline-1) :overline t))))

     ;; company-mode
     `(tooltip ((t (:inherit fixed-pitch))))
     `(company-tooltip ((t (:inherit tooltip))))
     `(company-tooltip-selection ((t (:weight bold))))
     `(company-tooltip-annotation ((t (:slant normal))))
     `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :slant normal :weight bold))))
     `(company-posframe-active-backend-name ((t (:inherit company-tooltip :background unspecified :weight bold))))
     `(company-posframe-inactive-backend-name ((t (:inherit company-tooltip :background unspecified))))

     ;; org with variable font
     `(org-level-8 ((t (:inherit (outline-8 variable-pitch)))))
     `(org-level-7 ((t (:inherit (outline-7 variable-pitch)))))
     `(org-level-6 ((t (:inherit (outline-6 variable-pitch)))))
     `(org-level-5 ((t (:inherit (outline-5 variable-pitch)))))
     `(org-level-4 ((t (:inherit (outline-4 variable-pitch) :height 1.25))))
     `(org-level-3 ((t (:inherit (outline-3 variable-pitch) :height 1.25))))
     `(org-level-2 ((t (:inherit (outline-2 variable-pitch) :height 1.25))))
     `(org-level-1 ((t (:inherit (outline-1 variable-pitch) :height 1.5))))
     `(org-document-title
       ((t (:inherit variable-pitch :foreground ,base-font-color :weight Bold :height 1.7 :underline t))))
     ;; org with fixed font
     `(org-ellipsis
       ((t (:inherit fixed-pitch))))
     `(org-meta-line
       ((t (:inherit (shadow fixed-pitch)))))
     `(org-table
       ((t (:inherit fixed-pitch))))
     `(org-formula
       ((t (:inherit fixed-pitch))))
     `(org-latex-and-related
       ((t (:inherit fixed-pitch))))
     `(org-link
       ((t (:inherit fixed-pitch :underline t))))
     `(org-special-keyword
       ((t (:inherit fixed-pitch))))
     `(org-checkbox
       ((t (:inherit fixed-pitch))))
     `(org-property-value
       ((t (:inherit fixed-pitch))))
     `(org-tag
       ((t (:inherit fixed-pitch :weight bold :height 0.8))))
     `(org-document-info
       ((t (:foreground "dark orange"))))
     `(org-document-info-keyword
       ((t (:inherit (shadow fixed-pitch)))))
     `(org-block
       ((t (:inherit fixed-pitch :background ,block-color :extend t))))
     `(org-block-begin-line
       ((t (:inherit org-block :foreground ,shadow-color :underline ,shadow-color))))
     `(org-block-end-line
       ((t (:inherit org-block-begin-line))))
     `(org-code
       ((t (:inherit fixed-pitch :background ,block-color))))
     `(org-verbatim
       ((t (:inherit fixed-pitch :background ,block-color))))

     ;; markdown with variable font
     `(markdown-header-face-6 ((t (:inherit (outline-6 variable-pitch)))))
     `(markdown-header-face-5 ((t (:inherit (outline-5 variable-pitch)))))
     `(markdown-header-face-4 ((t (:inherit (outline-4 variable-pitch) :height 1.25))))
     `(markdown-header-face-3 ((t (:inherit (outline-3 variable-pitch) :height 1.25))))
     `(markdown-header-face-2 ((t (:inherit (outline-2 variable-pitch) :height 1.25))))
     `(markdown-header-face-1 ((t (:inherit (outline-1 variable-pitch) :height 1.5))))
     `(markdown-metadata-value-face
       ((t (:inherit variable-pitch :foreground ,base-font-color :weight Bold :height 1.7 :underline t))))
     ;; markdown with fixed font
     `(markdown-metadata-key-face
       ((t (:inherit fixed-pitch :height 0.8))))
     `(markdown-header-delimiter-face
       ((t (:inherit (shadow fixed-pitch) :height 0.8))))
     `(markdown-language-info-face
       ((t (:inherit (shadow fixed-pitch)))))
     `(markdown-code-face
       ((t (:inherit fixed-pitch :background ,block-color :extend t))))
     `(markdown-markup-face
       ((t (:inherit (shadow fixed-pitch) :foreground unspecified :slant normal)))))))

;; * Load theme
(defun zw/theme-set-theme ()
  (let ((light-theme-params `((block-color . ,(doom-darken (face-background 'default) 0.06))
                              (mode-line-inactive-color . ,(doom-darken (face-background 'mode-line-inactive) 0.05))
                              (tab-bar-box . ,(doom-darken (face-background 'tab-bar) 0.05))))
        (dark-theme-params `((block-color . ,(doom-lighten (face-background 'default) 0.06))
                             (mode-line-inactive-color . ,(doom-lighten (face-background 'mode-line-inactive) 0.05))
                             (tab-bar-box . ,(doom-lighten (face-background 'tab-bar) 0.05)))))
    (pcase (frame-parameter nil 'background-mode)
      ('light (zw/theme--set-theme light-theme-params))
      ('dark (zw/theme--set-theme dark-theme-params)))))

;; temporary theme selector
(defvar zw/theme-selector (expand-file-name "zw-select-theme.el" user-emacs-directory))
(when (not (file-exists-p zw/theme-selector))
  (write-region "(load-theme 'doom-one t)" nil zw/theme-selector))
(load zw/theme-selector)

;; load custom faces
(zw/theme-set-theme)
(add-hook 'server-after-make-frame-hook 'zw/theme-set-theme)
(advice-add #'consult-theme
            :after (lambda (arg)
                     (zw/theme-set-theme)
                     (setq zw/modeline-bg (face-background 'mode-line))
                     (write-region (format "(load-theme '%s t)" (car custom-enabled-themes))
                                   nil zw/theme-selector)))

;; * Provide
(provide 'zw-theme)
