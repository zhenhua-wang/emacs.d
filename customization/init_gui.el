;; my old settings
;(add-to-list 'default-frame-alist '(width . 200))
;(add-to-list 'default-frame-alist '(height . 120))
;(toggle-scroll-bar -1)
;(tool-bar-mode -1)


;; new config starts
(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(width . 200))
;;(add-to-list 'default-frame-alist '(height . 120))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

;(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(xwidget-webkit-mode-hook
		org-mode-hook
		treemacs-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; starting
(setq initial-scratch-message (format ";; Welcome to emacs, %s!\n\n" user-login-name))
(add-hook 'emacs-startup-hook 'eshell)

;; padding
;;(lambda () (progn
;;  (setq left-margin-width 2)
;;  (setq right-margin-width 2)
;;  (set-window-buffer nil (current-buffer))))
