;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; eshell ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun efs/configure-eshell ()
  ;; Save command history when commands are entered
  (add-hook 'eshell-pre-command-hook 'eshell-save-some-history)

  ;; Truncate buffer for performance
  (add-to-list 'eshell-output-filter-functions 'eshell-truncate-buffer)

  ;; Use completion-at-point to provide completions in eshell
  (define-key eshell-mode-map (kbd "<tab>") 'completion-at-point)
  ;; (define-key eshell-mode-map [remap eshell-pcomplete] 'completion-at-point)

  (setenv "PAGER" "cat")

  (setq eshell-history-size         10000
        eshell-buffer-maximum-lines 10000
        eshell-hist-ignoredups t
        eshell-scroll-to-bottom-on-input t))

(use-package eshell-git-prompt
  :after eshell)

(use-package eshell
  :hook (eshell-first-time-mode . efs/configure-eshell)
  :config

  (with-eval-after-load 'esh-opt
    (setq eshell-destroy-buffer-when-process-dies t)
    (setq eshell-visual-commands '("htop" "zsh" "vim")))
  )

(load-file "~/.emacs.d/customization/esh-custom.el")

;; fish completion
(use-package fish-completion
  :hook (eshell-mode . fish-completion-mode))


(use-package esh-autosuggest
  :hook (eshell-mode . esh-autosuggest-mode)
  :config
  (setq esh-autosuggest-delay 0.5)
  (set-face-foreground 'company-preview-common "#4b5668")
  (set-face-background 'company-preview nil))

;; command highlight
(use-package eshell-syntax-highlighting
  :after esh-mode
  :config
  (eshell-syntax-highlighting-global-mode +1))

;;;;;;;;;;;;;;;;   custom command    ;;;;;;;;;;;;;;



(provide 'init-eshell)
