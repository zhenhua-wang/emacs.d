(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  (add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
  (add-to-list 'package-archives
	       '("melpa-stable" . "https://stable.melpa.org/packages/") t)
  (add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)
  (add-to-list 'package-archives '("elpa" . "https://elpa.gnu.org/packages/") t)
  ;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
  ;; and `package-pinned-packages`. Most users will not need or want to do this.
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  )
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

  ;; Initialize use-package on non-Linux platforms
(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes '(doom-one))
 '(custom-safe-themes
   '("835868dcd17131ba8b9619d14c67c127aa18b90a82438c8613586331129dda63" "c2aeb1bd4aa80f1e4f95746bda040aafb78b1808de07d340007ba898efa484f5" "4b0e826f58b39e2ce2829fab8ca999bcdc076dec35187bf4e9a4b938cb5771dc" "47db50ff66e35d3a440485357fb6acb767c100e135ccdf459060407f8baea7b2" "9b54ba84f245a59af31f90bc78ed1240fca2f5a93f667ed54bbf6c6d71f664ac" "a0be7a38e2de974d1598cf247f607d5c1841dbcef1ccd97cded8bea95a7c7639" "cf922a7a5c514fad79c483048257c5d8f242b21987af0db813d3f0b138dfaf53" "1704976a1797342a1b4ea7a75bdbb3be1569f4619134341bd5a4c1cfb16abad4" "4a5aa2ccb3fa837f322276c060ea8a3d10181fecbd1b74cb97df8e191b214313" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "fe2539ccf78f28c519541e37dc77115c6c7c2efcec18b970b16e4a4d2cd9891d" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "78c4238956c3000f977300c8a079a3a8a8d4d9fee2e68bad91123b58a4aa8588" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "939ea070fb0141cd035608b2baabc4bd50d8ecc86af8528df9d41f4d83664c6a" "123a8dabd1a0eff6e0c48a03dc6fb2c5e03ebc7062ba531543dfbce587e86f2a" default))
 '(ediff-diff-options "-w")
 '(ediff-split-window-function 'split-window-horizontally)
 '(ediff-window-setup-function 'ediff-setup-windows-plain)
 '(ein:output-area-inlined-images t)
 '(exwm-floating-border-color "#d6d4d4")
 '(fci-rule-color "#a3a1a1")
 '(highlight-tail-colors
   ((("#f0f3e5" "#669900" "green")
     . 0)
    (("#f3f8f7" "#8abeb7" "cyan")
     . 20)))
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#f2f2f2" "#4271ae"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#f2f2f2" "#718c00"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#f2f2f2" "#8e908c"))
 '(latex-preview-pane-use-frame nil)
 '(objed-cursor-color "#c82829")
 '(package-selected-packages
   '(company-prescient ivy-prescient eshell-git-prompt all-the-icons-ibuffer all-the-icons-ivy-rich all-the-icons-ivy all-the-icons-dired magit company-box python-mode command-log-mode doom-themes ivy-rich rainbow-delimiters counsel company-jedi org-preview-html ob-ipython gruvbox-theme org-bullets neotree doom-modeline latex-preview-pane ein grip-mode company ess julia-mode ## poly-R))
 '(pdf-view-midnight-colors '("#fdf4c1" . "#32302f"))
 '(python-indent-guess-indent-offset-verbose nil)
 '(rustic-ansi-faces
   ["#ffffff" "#c82829" "#718c00" "#eab700" "#4271ae" "#c678dd" "#8abeb7" "#4d4d4c"])
 '(vc-annotate-background "#ffffff")
 '(vc-annotate-color-map
   (list
    (cons 20 "#718c00")
    (cons 40 "#999a00")
    (cons 60 "#c1a800")
    (cons 80 "#eab700")
    (cons 100 "#eda70a")
    (cons 120 "#f19714")
    (cons 140 "#f5871f")
    (cons 160 "#e5825e")
    (cons 180 "#d57d9d")
    (cons 200 "#c678dd")
    (cons 220 "#c65da1")
    (cons 240 "#c74265")
    (cons 260 "#c82829")
    (cons 280 "#b94141")
    (cons 300 "#ab5c5a")
    (cons 320 "#9c7673")
    (cons 340 "#a3a1a1")
    (cons 360 "#a3a1a1")))
 '(vc-annotate-very-old-color nil))


;; *************** my customization ***********************
(add-to-list 'default-frame-alist
             '(ns-transparent-titlebar . t))

(add-to-list 'default-frame-alist
             '(ns-appearance . light)) ;; or dark - depending on your theme

;; if graph emacs then turn off toolbars
;; load my customization
(load-file "~/.emacs.d/customization/customization.el")

;; load my packages
(load-file "~/.emacs.d/customize-packages/htmlize.el")

;; load my theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")

;; load my key-bindings
(load-file "~/.emacs.d/customization/keybinds.el")

(when (display-graphic-p) 
    (load-file "~/.emacs.d/customization/init_gui.el"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-info ((t (:foreground "dark orange"))))
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))
 '(org-document-title ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro" :height 2.0 :underline nil))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-level-1 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro" :height 1.75))))
 '(org-level-2 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro" :height 1.5))))
 '(org-level-3 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro" :height 1.25))))
 '(org-level-4 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro" :height 1.1))))
 '(org-level-5 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro"))))
 '(org-level-6 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro"))))
 '(org-level-7 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro"))))
 '(org-level-8 ((t (:inherit default :weight bold :foreground "#bbc2cf" :font "Source Sans Pro"))))
 '(org-link ((t (:foreground "royal blue" :underline t))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-property-value ((t (:inherit fixed-pitch))) t)
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch :foreground "#83a598"))))
 '(org-tag ((t (:inherit (shadow fixed-pitch) :weight bold :height 0.8))))
 '(org-verbatim ((t (:inherit (shadow fixed-pitch)))))
 '(variable-pitch ((t (:family "ETBembo" :height 160)))))
