;; hide startup screen
(setq inhibit-startup-screen t)

;; starting
(setq initial-scratch-message (format ";; Welcome to emacs, %s! The %s!\n\n" user-login-name (sanityinc/show-init-time)))


;; set window frame
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

;; Set up the visible bell
(setq visible-bell t)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(xwidget-webkit-mode-hook
		org-mode-hook
		treemacs-mode-hook
		neotree-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))


;;; Scrolling.
;; Good speed and allow scrolling through large images (pixel-scroll).
(pixel-scroll-mode)
(setq pixel-dead-time 0) ; Never go back to the old scrolling behaviour.
(setq pixel-resolution-fine-flag t) ; Scroll by number of pixels instead of lines (t = frame-char-height pixels).
(setq mouse-wheel-scroll-amount '(1)) ; Distance in pixel-resolution to scroll each mouse wheel event.
(setq mouse-wheel-progressive-speed nil) ; Progressive speed is too fast for me.
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; packages ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-modeline
  :ensure t
  :config
  (doom-modeline-mode 1)
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq doom-modeline-github t))

(use-package which-key
  :init (which-key-mode)
  :diminish which-key-mode
  :config
  (setq which-key-idle-delay 0.3))



(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package command-log-mode :ensure t)

(use-package neotree :ensure t)

(use-package spacegray-theme :ensure t)
(use-package doom-themes :ensure t)
(use-package gruvbox-theme :ensure t)

(provide 'init-gui)
