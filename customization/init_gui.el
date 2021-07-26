;; my old settings
;(add-to-list 'default-frame-alist '(width . 200))
;(add-to-list 'default-frame-alist '(height . 120))
;(toggle-scroll-bar -1)
;(tool-bar-mode -1)


;; new config starts
(add-to-list 'default-frame-alist '(width . 200))
(add-to-list 'default-frame-alist '(height . 120))

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 10)        ; Give some breathing room

(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; line numbers
(column-number-mode)
(global-display-line-numbers-mode t)
