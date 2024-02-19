;; -*- lexical-binding: t -*-

;; * Font
(let ((default-font (font-spec :name "Noto Sans Mono" :size 15.0))
      (cn-font (font-spec :name "Noto Sans Mono CJK SC" :size 18.0))
      (emoji-font (font-spec :name "Noto Color Emoji" :size 18.0)))
  (set-face-attribute 'default nil :font default-font)
  (dolist (charset '(kana han cjk-misc bopomofo))
    (set-fontset-font t charset cn-font))
  (set-fontset-font t 'symbol emoji-font))

;; * Doom theme
(use-package doom-themes
  :defer t
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil))

;; * ZW theme
(defun zw/theme--set-theme (theme-params)
  (let* ((base-font-color         (face-foreground 'default nil 'default))
         (fixed-font             `(:font "JetBrains Mono"))
         (variable-font          `(:font "EB Garamond" :weight SemiBold))
         (modeline-height         130)
         (tab-bar-height          120)
         (block-bg (alist-get 'block-bg theme-params))
         (modeline-highlight-bg (alist-get 'modeline-highlight-bg theme-params))
         (modeline-highlight-fg (alist-get 'modeline-highlight-fg theme-params))
         (modeline-highlight-inactive-bg (alist-get 'modeline-highlight-inactive-bg theme-params))
         (tab-bar-box (alist-get 'tab-bar-box theme-params)))
    (custom-theme-set-faces
     'user
     ;; fonts
     `(fixed-pitch ((t (,@fixed-font))))
     `(variable-pitch ((t (,@variable-font))))

     ;; child frame
     `(child-frame-border ((t (:background ,(face-background 'highlight)))))

     ;; modeline
     `(mode-line
       ((t (:height ,modeline-height))))
     `(mode-line-inactive
       ((t (:inherit mode-line :foreground ,(face-foreground 'font-lock-comment-face)))))
     `(mode-line-highlight
       ((t (:inherit mode-line :foreground ,modeline-highlight-fg :background ,modeline-highlight-bg))))
     `(zw/modeline-default-active
       ((t (:height ,modeline-height :foreground ,(face-foreground 'mode-line)))))
     `(zw/modeline-default-inactive
       ((t (:height ,modeline-height :foreground ,(face-foreground 'font-lock-comment-face)))))
     `(zw/modeline-modified-active
       ((t (:inherit (warning zw/modeline-buffer-name-active)))))
     `(zw/modeline-highlight-foreground-active
       ((t (:foreground ,(face-background 'highlight)))))
     `(zw/modeline-highlight-background-inactive
       ((t (:inherit zw/modeline-default-inactive :background ,modeline-highlight-inactive-bg))))
     `(zw/modeline-separator-active
       ((t :background ,(face-background 'mode-line))))

     ;; tab-bar
     `(tab-bar
       ((t (:foreground ,base-font-color :height ,tab-bar-height :weight regular :box ,tab-bar-box))))
     `(zw/tab-bar-default-selected
       ((t (:inherit tab-bar))))
     `(zw/tab-bar-menu-bar
       ((t (:inherit zw/tab-bar-default-selected :bold t))))
     `(zw/tab-bar-tab-path-selected
       ((t (:inherit zw/tab-bar-default-selected :bold t :foreground ,modeline-highlight-bg))))
     `(zw/tab-bar-tab-battery-load-default
       ((t (:inherit zw/tab-bar-default-selected :bold t))))
     `(zw/tab-bar-tab-battery-load-charging
       ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'success) :bold t))))
     `(zw/tab-bar-tab-battery-load-low
       ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'warning) :bold t))))
     `(zw/tab-bar-tab-battery-load-critical
       ((t (:inherit zw/tab-bar-default-selected :foreground ,(face-foreground 'error) :bold t))))

     ;; tab-line
     `(tab-line ((t (:background ,(face-background 'tab-bar nil 'default) :underline ,tab-bar-box))))

     ;; header-line
     `(header-line ((t (:background ,(face-background 'mode-line) :underline ,tab-bar-box :bold t))))

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
     `(show-paren-match ((t (:background ,(face-foreground 'warning) :foreground "black" :weight extra-bold))))

     ;; diredfl
     `(diredfl-dir-name ((t (:bold t))))

     ;; outline minor
     `(outline-minor-1 ((t (:inherit (outline-minor-0 outline-1) :overline t))))

     ;; company-mode
     `(company-tooltip ((t (:inherit tooltip ,@fixed-font))))
     `(company-tooltip-selection ((t (:weight bold))))
     `(company-tooltip-annotation ((t (:slant normal))))
     `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :slant normal :weight bold))))
     `(company-posframe-active-backend-name ((t (:inherit company-tooltip :background unspecified :weight bold))))
     `(company-posframe-inactive-backend-name ((t (:inherit company-tooltip :background unspecified))))

     ;; org with variable font
     `(org-level-8 ((t (:inherit outline-8 ,@variable-font))))
     `(org-level-7 ((t (:inherit outline-7 ,@variable-font))))
     `(org-level-6 ((t (:inherit outline-6 ,@variable-font))))
     `(org-level-5 ((t (:inherit outline-5 ,@variable-font))))
     `(org-level-4 ((t (:inherit outline-4 ,@variable-font :height 1.25))))
     `(org-level-3 ((t (:inherit outline-3 ,@variable-font :height 1.25))))
     `(org-level-2 ((t (:inherit outline-2 ,@variable-font :height 1.25))))
     `(org-level-1 ((t (:inherit outline-1 ,@variable-font :height 1.5))))
     `(org-document-title
       ((t (,@variable-font :foreground ,base-font-color :weight Bold :height 1.7 :underline t))))
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
       ((t (:inherit (shadow fixed-pitch)))))
     `(org-link
       ((t (:inherit fixed-pitch :underline t))))
     `(org-special-keyword
       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     `(org-checkbox
       ((t (:inherit 'fixed-pitch))))
     `(org-property-value
       ((t (:inherit fixed-pitch))))
     `(org-tag
       ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
     `(org-document-info
       ((t (:foreground "dark orange"))))
     `(org-document-info-keyword
       ((t (:inherit (shadow fixed-pitch)))))
     `(org-block
       ((t (:inherit fixed-pitch :background ,block-bg))))
     `(org-block-begin-line
       ((t (:inherit 'fixed-pitch :background ,block-bg :bold t :italic t :underline t :extend t))))
     `(org-block-end-line
       ((t (:background ,block-bg :bold t :italic t :extend t))))
     `(org-code
       ((t (:inherit (shadow fixed-pitch) :background ,block-bg))))
     `(org-verbatim
       ((t (:inherit (shadow fixed-pitch) :background ,block-bg))))

     ;; markdown with variable font
     `(markdown-header-face-6 ((t (:inherit outline-6 ,@variable-font))))
     `(markdown-header-face-5 ((t (:inherit outline-5 ,@variable-font))))
     `(markdown-header-face-4 ((t (:inherit outline-4 ,@variable-font :height 1.25))))
     `(markdown-header-face-3 ((t (:inherit outline-3 ,@variable-font :height 1.25))))
     `(markdown-header-face-2 ((t (:inherit outline-2 ,@variable-font :height 1.25))))
     `(markdown-header-face-1 ((t (:inherit outline-1 ,@variable-font :height 1.5))))
     `(markdown-metadata-value-face
       ((t (,@variable-font :foreground ,base-font-color :weight Bold :height 1.7 :underline t))))
     ;; markdown with fixed font
     `(markdown-metadata-key-face
       ((t (:inherit (thin fixed-pitch) :height 0.8))))
     `(markdown-header-delimiter-face
       ((t (:inherit (font-lock-comment-face fixed-pitch) :height 0.8))))
     `(markdown-language-info-face
       ((t (:inherit (font-lock-comment-face fixed-pitch)))))
     `(markdown-code-face
       ((t (:inherit fixed-pitch :background ,block-bg :extend t))))
     `(markdown-markup-face
       ((t (:inherit (font-lock-comment-face fixed-pitch) :foreground unspecified :slant normal)))))))

;; * Load theme
(defun zw/theme-set-theme ()
  (let ((light-theme-params `((block-bg . ,(doom-darken (face-background 'default) 0.06))
                              (modeline-highlight-bg . ,(face-background 'highlight))
                              (modeline-highlight-fg . ,(face-foreground 'highlight))
                              (modeline-highlight-inactive-bg . ,(doom-darken (face-background 'mode-line-inactive)
                                                                              0.05))
                              (tab-bar-box . ,(doom-darken (face-background 'tab-bar) 0.05))))
        (dark-theme-params `((block-bg . ,(doom-lighten (face-background 'default) 0.06))
                             (modeline-highlight-bg . ,(face-background 'highlight))
                             (modeline-highlight-fg . ,(face-foreground 'highlight))
                             (modeline-highlight-inactive-bg . ,(doom-lighten (face-background 'mode-line-inactive)
                                                                              0.05))
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
