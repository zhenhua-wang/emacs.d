;; -*- lexical-binding: t -*-

;; * Nerd icon
(use-package nerd-icons
  :config
  (with-eval-after-load "zw-theme"
    (nerd-icons-set-font))
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
                         'prepend))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-completion
  :after (marginalia nerd-icons)
  :hook
  (marginalia-mode . nerd-icons-completion-marginalia-setup)
  (marginalia-mode . nerd-icons-completion-mode))

;; * Posframe
(use-package posframe
  :if (zw/icon-displayable-p)
  :defer t)

;; * Rain bow delimiters
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; * Rainbow mode
;; Sets the background of HTML color strings in buffers to be the color mentioned.
(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode)
  (text-mode . rainbow-mode))

;; * Pulsar
(use-package pulsar
  :init
  (setq pulsar-pulse t
        pulsar-delay 0.055
        pulsar-iterations 10
        pulsar-face 'pulsar-red
        pulsar-highlight-face 'pulsar-face)
  :hook
  (after-init . pulsar-global-mode)
  ;; integration with the `consult' package:
  (consult-after-jump . pulsar-recenter-top)
  (consult-after-jump . pulsar-reveal-entry)
  ;; integration with the built-in `imenu':
  (imenu-after-jump . pulsar-recenter-top)
  (imenu-after-jump . pulsar-reveal-entry))

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
(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
         (diff-hl-mode . diff-hl-flydiff-mode)
         (dired-mode . (lambda ()
                         (unless (file-remote-p default-directory)
                           (diff-hl-dired-mode 1)))))
  :init (setq diff-hl-side 'left
              diff-hl-draw-borders nil
              diff-hl-show-staged-changes nil)
  :config
  ;; Integration with magit
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

;; * Indent guide
(use-package indent-guide
  :hook (python-mode . indent-guide-mode))

;; * Provide
(provide 'zw-appearance)
