;; -*- lexical-binding: t -*-

;; * Font
(defun zw/theme-set-font ()
  (let ((default-font (font-spec :name "Adwaita Mono" :size 14.0))
        (cn-font (font-spec :name "Noto Sans Mono CJK SC" :size 13.0))
        (emoji-font (font-spec :name "Noto Color Emoji" :size 13.0))
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
  ;; load nerd icons
  (ignore-errors (nerd-icons-set-font)))

(use-package nerd-icons
  :config
  (zw/merge-list-symbols 'nerd-icons/mdicon-alist
                         '(("nf-md-firefox_web_browser" . "󰈹")
                           ("nf-md-visual_studio_code" . "󰨞"))
                         'prepend)
  (zw/merge-list-symbols 'nerd-icons-regexp-icon-alist
                         '(("^firefox:.*" nerd-icons-mdicon "nf-md-firefox")
                           ("^discord:.*" nerd-icons-mdicon "nf-md-discord")
                           ("^Code:.*" nerd-icons-mdicon "nf-md-visual_studio_code"))
                         'prepend)
  (zw/merge-list-symbols 'nerd-icons-extension-icon-alist
                         '(("rmd" nerd-icons-octicon "nf-oct-markdown" :face nerd-icons-lblue))
                         'prepend)
  (zw/merge-list-symbols 'nerd-icons-mode-icon-alist
                         '((ess-r-mode nerd-icons-sucicon "nf-seti-r" :face nerd-icons-lblue))
                         'prepend)
  (advice-add 'zw/install-fonts :after (lambda () (nerd-icons-install-fonts t))))

(use-package nerd-icons-completion
  :hook
  ;; trigger autoload nerd-icons from here
  (vertico-mode . nerd-icons-completion-mode)
  (marginalia-mode . nerd-icons-completion-marginalia-setup))

;; * Default theme
(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic nil))

(use-package adwaita-dark-theme
  :vc (:url "https://github.com/zhenhua-wang/zw-adwaita-theme"))

(defun zw/theme-emphasize-color (color aplha dark-p)
  (if dark-p
      (doom-lighten color aplha)
    (doom-darken color aplha)))

;; * ZW theme
(defvar zw/after-set-theme-hook nil
  "Hook that gets run when after zw theme has been set.")
(defun zw/theme-set-theme ()
  (let* ((dark-p                      (eq (frame-parameter nil 'background-mode) 'dark))
         (base-font-color             (face-foreground 'default nil 'default))
         (ui-color                    (face-foreground 'mode-line nil 'default))
         (mode-line-color             (face-background 'mode-line nil 'default))
         (tab-bar-color               (face-background 'tab-bar nil 'default))
         (shadow-color                (face-foreground 'shadow nil 'default))
         (highlight-color             (or (face-background 'mode-line-highlight)
                                          (face-background 'highlight nil 'default)))
         (highlight-revert-color      (if (face-background 'mode-line-highlight)
                                          (face-foreground 'mode-line-highlight nil t)
                                        (face-foreground 'highlight nil 'default)))
         (highlight-alt-color         (face-foreground 'warning nil 'default))
         (highlight-alt-revert-color  highlight-revert-color)
         (highlight-inactive-color    (zw/theme-emphasize-color
                                       (face-background 'mode-line-inactive nil t) 0.05 dark-p))
         (block-color                 (zw/theme-emphasize-color
                                       (face-background 'default nil t) 0.06 dark-p))
         (popup-color                 (zw/theme-emphasize-color
                                       (face-background 'default nil t) 0.1 dark-p))
         (popup-hl-color              (zw/theme-emphasize-color
                                       (face-background 'default nil t) 0.2 dark-p))
         (tab-bar-box                 (zw/theme-emphasize-color
                                       (face-background 'tab-bar nil t) 0.05 dark-p))
         (modeline-height             130)
         (tab-bar-height              120))
    ;; base face
    (set-face-attribute 'tooltip nil
                        :inherit 'fixed-pitch :foreground base-font-color)
    (set-face-attribute 'mode-line nil
                        :height modeline-height :box 'unspecified)
    (set-face-attribute 'mode-line-inactive nil
                        :inherit 'mode-line :foreground shadow-color :box 'unspecified)
    (set-face-attribute 'tab-bar nil
                        :inherit 'unspecified :foreground ui-color :height tab-bar-height
                        :weight 'regular :box tab-bar-box)
    (set-face-attribute 'tab-line nil
                        :inherit 'unspecified :foreground ui-color :background tab-bar-color
                        :underline tab-bar-box)
    (set-face-attribute 'header-line nil
                        :inherit 'unspecified :foreground ui-color :background mode-line-color
                        :underline tab-bar-box :bold t)
    (set-face-attribute 'child-frame-border nil
                        :background highlight-color)
    (set-face-attribute 'zw/left-side-window-face nil
                        :height (face-attribute 'default :height))
    ;; user face
    (custom-theme-set-faces
     'user
     ;; modeline
     `(zw/modeline-default-active
       ((t (:height ,modeline-height))))
     `(zw/modeline-default-inactive
       ((t (:height ,modeline-height :foreground ,shadow-color))))
     `(zw/modeline-modified-active
       ((t (:inherit zw/modeline-buffer-name-active :foreground ,highlight-alt-color))))
     `(zw/modeline-highlight-foreground-active
       ((t (:inherit mode-line :foreground ,highlight-color))))
     `(zw/modeline-highlight-background-inactive
       ((t (:inherit zw/modeline-default-inactive :background ,highlight-inactive-color))))
     `(zw/modeline-separator-active
       ((t (:inherit mode-line :background ,mode-line-color))))
     `(zw/modeline-local-active
       ((t (:inherit mode-line :foreground ,highlight-revert-color :background ,highlight-color))))
     `(zw/modeline-remote-active
       ((t (:inherit mode-line :foreground ,highlight-alt-revert-color :background ,highlight-alt-color))))
     `(zw/modeline-major-mode-active
       ((t (:inherit zw/modeline-default-active :bold t))))
     `(zw/modeline-lsp-active
       ((t (:inherit success))))

     ;; tab-bar
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

     ;; tty menu
     `(tty-menu-enabled-face
       ((t (:background ,(face-background 'tooltip)))))
     `(tty-menu-disabled-face
       ((t (:background ,(face-background 'tooltip) :foreground ,(face-foreground 'tooltip)))))
     `(tty-menu-selected-face
       ((t (:background ,highlight-color :foreground ,highlight-revert-color :weight bold))))

     ;; vc
     '(vc-edited-state ((t (:foreground "#FF9F29"))))
     '(vc-locally-added-state ((t (:foreground "#3CCF4E"))))
     '(vc-removed-state ((t (:foreground "#E94560"))))

     ;; diff-hl
     `(diff-hl-change ((t (:foreground "black" :background "#FF9F29"))))
     `(diff-hl-insert ((t (:foreground "black" :background "#3CCF4E"))))
     `(diff-hl-delete ((t (:foreground "white" :background "#E94560"))))

     ;; key-cast
     `(keycast-key ((t (:height ,tab-bar-height :inherit (highlight bold)))))
     `(keycast-command ((t (:height ,tab-bar-height))))

     ;; show paren
     `(show-paren-match ((t (:background ,highlight-alt-color :foreground ,highlight-alt-revert-color :weight bold))))

     ;; diredfl
     `(diredfl-dir-name ((t (:bold t))))

     ;; outline minor
     `(outline-minor-0 ((t (:background ,tab-bar-color))))
     `(outline-minor-1 ((t (:inherit (outline-minor-0 outline-1) :overline t))))

     ;; company-mode
     `(company-tooltip ((t (:inherit tooltip))))
     `(company-tooltip-selection ((t (:weight bold))))
     `(company-tooltip-annotation ((t (:slant normal))))
     `(company-tooltip-annotation-selection ((t (:inherit company-tooltip-annotation :slant normal :weight bold))))
     `(company-posframe-active-backend-name ((t (:inherit company-tooltip :background unspecified :weight bold))))
     `(company-posframe-inactive-backend-name ((t (:inherit company-tooltip :background unspecified))))

     ;; vertico
     `(vertico-posframe ((t (:inherit default :background ,popup-color))))
     `(vertico-posframe-border ((t (:inherit default :background ,popup-color))))
     `(vertico-current ((t (:inherit default :background ,popup-hl-color :bold t))))

     ;; eglot
     `(eglot-highlight-symbol-face
       ((t (:inherit bold :background ,highlight-color :foreground ,highlight-revert-color))))

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
       ((t (:inherit (shadow fixed-pitch) :foreground unspecified))))
     `(org-table
       ((t (:inherit fixed-pitch))))
     `(org-formula
       ((t (:inherit fixed-pitch))))
     `(org-latex-and-related
       ((t (:inherit fixed-pitch))))
     `(org-link
       ((t (:inherit (link fixed-pitch) :underline t))))
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
       ((t (:inherit org-block :foreground ,shadow-color))))
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
       ((t (:inherit (shadow fixed-pitch) :foreground unspecified :slant normal))))))
  ;; after hook
  (run-hooks 'zw/after-set-theme-hook))

;; * Load theme
(defun zw/theme-load-ui ()
  (zw/theme-set-font)
  (zw/theme-set-theme))
;; temporary theme selector
(defvar zw/theme-selector-name (if (display-graphic-p) "zw-select-theme.el" "zw-select-theme-tty.el"))
(defvar zw/theme-selector (expand-file-name zw/theme-selector-name user-emacs-directory))
;; indicator for whether to initialize theme
(defvar zw/theme-init-p t)
(defvar zw/theme-init-before-hook nil)
(add-hook 'after-init-hook
          (lambda ()
            (run-hooks 'zw/theme-init-before-hook)
            (when zw/theme-init-p
              ;; default theme
              (when (not (file-exists-p zw/theme-selector))
                (write-region "(load-theme 'adwaita-dark t)" nil zw/theme-selector))
              ;; load theme
              (ignore-errors (load zw/theme-selector)))
            (zw/theme-load-ui)))

;; load custom faces
(when (daemonp)
  (add-hook 'server-after-make-frame-hook #'zw/theme-load-ui))
(advice-add #'consult-theme
            :after (lambda (arg)
                     (zw/theme-set-theme)
                     (setq zw/modeline-bg (face-background 'mode-line nil t))
                     (let ((default-theme (car custom-enabled-themes)))
                       (if default-theme
                           (write-region (format "(load-theme '%s t)" default-theme)
                                         nil zw/theme-selector)
                         (write-region "(set-face-attribute 'mode-line-highlight nil :inherit 'unspecified)"
                                       nil zw/theme-selector)))))

;; * Provide
(provide 'zw-theme)
