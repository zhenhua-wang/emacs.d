;; -*- lexical-binding: t -*-

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
         (diff-hl-mode . zw/diff-hl-init)
         (dired-mode . (lambda ()
                         (unless (file-remote-p default-directory)
                           (diff-hl-dired-mode 1)))))
  :init (setq diff-hl-side 'right
              diff-hl-draw-borders nil
              diff-hl-show-staged-changes nil)
  :config
  ;; Integration with magit
  (with-eval-after-load "magit"
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))
  ;; Fall back to margin when in terminal
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (defun zw/diff-hl-init ()
    (setq right-fringe-width 25)))

;; * Indent guide
(use-package indent-guide
  :hook (python-mode . indent-guide-mode))

;; * Provide
(provide 'zw-appearance)
