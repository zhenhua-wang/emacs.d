;; hide startup screen
(setq-default inhibit-startup-screen t
	      cursor-in-non-selected-windows nil)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(menu-bar-mode -1)            ; Disable the menu bar

;; Set up the visible bell
(setq visible-bell t)

;; Scrolling
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse
(setq scroll-step 1) ;; keyboard scroll one line at a time
(setq use-dialog-box nil) ;; Disable dialog boxes since they weren't working in Mac OSX

;; Set frame
(pcase system-type
  ('gnu/linux (
  	       (lambda ()
                 (set-frame-parameter (selected-frame) 'fullscreen 'maximized)
                 (add-to-list 'default-frame-alist '(fullscreen . maximized)))))
  ('darwin (
            (lambda ()
              (add-to-list 'initial-frame-alist '(fullscreen . maximized))
              (add-to-list 'default-frame-alist '(fullscreen . maximized))
              ;; no title bar for mac
	      (add-to-list 'default-frame-alist '(undecorated . t))
              ;; transparency
              (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
              (add-to-list 'default-frame-alist '(alpha . (100 . 100))))))
    ('windows-nt (
            (lambda ()
              (add-to-list 'initial-frame-alist '(fullscreen . maximized))
              (add-to-list 'default-frame-alist '(fullscreen . maximized))))))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)
