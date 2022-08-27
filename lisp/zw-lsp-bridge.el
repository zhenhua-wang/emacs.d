(use-package lsp-bridge
  :straight (lsp-bridge :host github :repo "manateelazycat/lsp-bridge"
                        :files ("*"))
  :hook
  (python-mode . lsp-bridge-mode)
  (ess-r-mode . lsp-bridge-mode)
  :config
  (setq corfu-excluded-modes '(python-mode ess-r-mode))
  (setq acm-enable-doc nil
        acm-candidate-match-function 'orderless-prefixes)
  ;; acm keys
  (define-key acm-mode-map (kbd "M->") 'acm-select-last)
  (define-key acm-mode-map (kbd "M-<") 'acm-select-first)
  ;; acm font
  (set-face-attribute 'acm-select-face nil
		      :bold t
		      :foreground (face-foreground
				   'default)
		      :background (face-background
				   'modus-themes-completion-selected-popup)))
