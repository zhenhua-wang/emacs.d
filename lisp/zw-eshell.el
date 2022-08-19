;; zw-eshell.el --- Initialize eshell configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Eshell.
;;

;;; Code:

(use-package eshell
  :straight (:type built-in)
  :init
  (setq eshell-banner-message
        '(format "%s %s\n"
                 (propertize (format " %s " (string-trim (buffer-name)))
                             'face 'mode-line-highlight)
                 (propertize (current-time-string)
                             'face 'font-lock-keyword-face))
        eshell-scroll-to-bottom-on-input 'all
        eshell-scroll-to-bottom-on-output nil
        eshell-kill-processes-on-exit t
        eshell-hist-ignoredups t
        ;; don't record command in history if prefixed with whitespace
        eshell-input-filter (lambda (input) (not (string-match-p "\\`\\s-+" input)))
        ;; em-glob
        eshell-glob-case-insensitive t
        eshell-error-if-no-glob t)
  :config
  (add-hook 'eshell-mode-hook
            (lambda ()
              (define-key eshell-mode-map (kbd "s-q") #'zw/close-shell)
              (define-key eshell-mode-map (kbd "s-e") #'delete-window)
              (define-key eshell-hist-mode-map (kbd "M-s") nil))))

(use-package eshell-prompt-extras
  :after esh-opt
  :defines eshell-highlight-prompt
  :commands (epe-theme-lambda epe-theme-dakrone epe-theme-pipeline)
  :init (setq eshell-highlight-prompt t
              eshell-prompt-function #'epe-theme-lambda))

(use-package eshell-syntax-highlighting
  :hook (eshell-mode . eshell-syntax-highlighting-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zw-eshell.el ends here
