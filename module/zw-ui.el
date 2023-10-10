;; -*- lexical-binding: t -*-

;; * Nerd icon
(use-package nerd-icons
  :if (zw/icon-displayable-p)
  :config
  (zw/merge-list-to-list 'nerd-icons/mdicon-alist
                         '(("nf-md-firefox_web_browser" . "󰈹")
                           ("nf-md-visual_studio_code" . "󰨞"))
                         'prepend)
  (zw/merge-list-to-list 'nerd-icons-regexp-icon-alist
                         '(("^firefox:.*" nerd-icons-mdicon "nf-md-firefox")
                           ("^discord:.*" nerd-icons-mdicon "nf-md-discord")
                           ("^Code:.*" nerd-icons-mdicon "nf-md-visual_studio_code"))
                         'prepend)
  (zw/merge-list-to-list 'nerd-icons-extension-icon-alist
                         '(("rmd" nerd-icons-octicon "nf-oct-markdown" :face nerd-icons-lblue))
                         'prepend)
  (zw/merge-list-to-list 'nerd-icons-mode-icon-alist
                         '((ess-r-mode nerd-icons-sucicon "nf-seti-r" :face nerd-icons-lblue))
                         'prepend))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :if (zw/icon-displayable-p)
  :after (marginalia nerd-icons)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  (marginalia-mode . nerd-icons-completion-mode))

;; * Buffer face mode
;; Set fixed-font faces for prog
(add-hook 'prog-mode-hook
          (lambda ()
            (setq-local buffer-face-mode-face 'fixed-pitch)
            (buffer-face-mode)))

;; * Scroll
(setq scroll-step 0
      scroll-margin 1
      scroll-conservatively 101
      scroll-preserve-screen-position t
      mouse-wheel-scroll-amount '(2 ((shift) . hscroll))
      mouse-wheel-scroll-amount-horizontal 2
      mouse-wheel-progressive-speed nil
      auto-window-vscroll nil
      fast-but-imprecise-scrolling t)

(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode t)
  (setq touch-screen-precision-scroll t)
  (bind-keys :map pixel-scroll-precision-mode-map
             ("<prior>" . nil)
             ("<next>" . nil)))

(use-package iscroll
  :diminish
  :hook
  (image-mode . iscroll-mode)
  (org-mode . iscroll-mode)
  (markdown-mode . iscroll-mode))

;; * Posframe
(use-package posframe :defer t)
;; * Line number mode
;; line number mode
(dolist (mode '(prog-mode-hook text-mode-hook conf-mode-hook))
  (add-hook mode 'display-line-numbers-mode))
;; Override some modes which derive from the above
(dolist (mode '(org-mode-hook markdown-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; * Rain bow delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; * Rainbow mode
;; Sets the background of HTML color strings in buffers to be the color mentioned.
(use-package rainbow-mode
  :diminish
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

;; * Pulsar
(use-package pulsar
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 10
        pulsar-face 'pulsar-generic
        pulsar-highlight-face 'pulsar-yellow)
  :hook
  (after-init . pulsar-global-mode)
  ;; integration with the `consult' package:
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)

  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry))

;; * Paren
;; Highlight matching parens
(use-package paren
  :straight (:type built-in)
  :hook (after-init . show-paren-mode)
  :config
  (setq show-paren-when-point-inside-paren nil
        show-paren-when-point-in-periphery nil
        show-paren-context-when-offscreen 'child-frame)
  (add-to-list 'show-paren--context-child-frame-parameters '(child-frame-border-width . 4)))

;; * Highlight line
;; Highlight the current line
(use-package hl-line
  :straight (:type built-in)
  :hook ((after-init . global-hl-line-mode)
         ((dashboard-mode eshell-mode shell-mode term-mode vterm-mode) .
          (lambda () (setq-local global-hl-line-mode nil)))))

;; * Highlight TODO
;; Highlight TODO and similar keywords in comments and strings
(use-package hl-todo
  :custom-face
  (hl-todo ((t (:inherit fixed-pitch :height 0.9 :width condensed :weight bold :underline nil :inverse-video t))))
  :hook (after-init . global-hl-todo-mode)
  ;; :init (setq hl-todo-require-punctuation t
  ;; hl-todo-highlight-punctuation ":")
  :config
  (dolist (keyword '("BUG" "DEFECT" "ISSUE"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#e45649")))
  (dolist (keyword '("TRICK" "WORKAROUND"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#d0bf8f")))
  (dolist (keyword '("DEBUG" "STUB"))
    (add-to-list 'hl-todo-keyword-faces `(,keyword . "#7cb8bb"))))

;; * Highlight VC
;; Highlight uncommitted changes using VC
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode)
         (dired-mode . diff-hl-dired-mode))
  :init (setq diff-hl-side 'left
              diff-hl-draw-borders nil
              diff-hl-show-staged-changes nil)
  :config
  ;; Integration with magit
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; * Window placement
;; window split
(setq split-width-threshold  80
      split-height-threshold 80
      split-window-preferred-function 'split-window-sensibly)

(defun zw/display-buffer-in-largest-window (buffer alist)
  (let ((largest-window (get-largest-window (selected-frame) t)))
    (window--display-buffer buffer largest-window 'reuse alist)))

;; default buffer placement rules
(setq display-buffer-base-action '((display-buffer--maybe-same-window
                                    zw/display-buffer-in-largest-window)))

;; popup buffers
(dolist (mode '(magit-mode-hook
                git-commit-setup-hook))
  (add-hook mode
            (lambda () (setq-local display-buffer-base-action
                                   '((display-buffer--maybe-same-window
                                      display-buffer--maybe-pop-up-frame-or-window))))))

;; buffer placement rules
(setq display-buffer-alist
      '(;; largest window
        ("\\.\\(?:pdf\\)\\'"
         (display-buffer-reuse-window
          zw/display-buffer-in-largest-window))
        ("\\*\\([Hh]elp\\|Man\\|eglot doc\\).*"
         (zw/display-buffer-in-largest-window))
        ;; top side window
        ("\\*\\(Messages\\|Warnings\\|Backtrace\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.2)
         (window-width . 0.5)
         (side . top)
         (slot . -1))
        ("\\*\\(polymode export\\|compilation\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.2)
         (window-width . 0.5)
         (side . top)
         (slot . 1))
        ;; right side window
        ("\\*\\(R\\|Python\\).*"
         (display-buffer-in-side-window)
         (window-width . 0.3)
         (side . right)
         (slot . 1)
         (dedicated . t))
        ;; bottom side buffer
        ("\\*.*\\(e?shell\\|v?term\\).*"
         (display-buffer-in-side-window)
         (window-height . 0.2)
         (side . bottom)
         (dedicated . t))
        ;; below current window
        ("\\*Calendar.*"
         (display-buffer-reuse-mode-window display-buffer-below-selected)
         (window-height . shrink-window-if-larger-than-buffer))))

;; * Whitespace
(dolist (mode '(prog-mode-hook
                text-mode-hook))
  (add-hook mode (lambda ()
                   (if buffer-file-name
                       (setq-local show-trailing-whitespace t)))))

;; * Side window
(defcustom zw/side-window-buffer-mode '(inferior-ess-r-mode inferior-python-mode)
  "List of modes of buffer displayed in side window.")

(defcustom zw/side-window-buffer-regex nil
  "List of name regex of buffer displayed in side window.")

(defvar zw/side-window--buffer-opened nil)

(defun zw/side-window--update ()
  (setq zw/side-window--buffer-opened nil)
  (let* ((buffers (buffer-list)))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (if (or (member major-mode zw/side-window-buffer-mode)
                (cl-some (lambda (regex)
                           (string-match-p regex (buffer-name buffer)))
                         zw/side-window-buffer-regex))
            (add-to-list 'zw/side-window--buffer-opened buffer))))))

(defun zw/side-window-toggle ()
  "Toggle side windows."
  (interactive)
  (zw/side-window--update)
  (if zw/side-window--buffer-opened
      (if (cl-some (lambda (buffer) (get-buffer-window buffer))
                   zw/side-window--buffer-opened)
          (dolist (buffer zw/side-window--buffer-opened)
            (let ((buffer-window (get-buffer-window buffer)))
              (when buffer-window
                (if  (eq buffer-window (window-main-window))
                    (previous-buffer)
                  (delete-window buffer-window)))))
        (dolist (buffer zw/side-window--buffer-opened)
          (display-buffer buffer)))
    (message "No buffer in side window.")))

;; * Provide
(provide 'zw-ui)
