;; -*- lexical-binding: t -*-

;;; code

(use-package treemacs
  :commands (treemacs treemacs-current)
  :config
  (defalias 'treemacs-current 'treemacs-display-current-project-exclusively)
  (treemacs-follow-mode t)
  (treemacs-filewatch-mode t))

(use-package lsp-treemacs
  :after lsp-mode
  :hook (lsp-mode . lsp-treemacs-sync-mode)
  :commands lsp-treemacs-errors-list)

(use-package dap-mode
  :bind ((:map dap-mode-map
               ("<f9> d" . dap-debug)
               ("<f9> h" . dap-hydra)))
  :hook
  (after-init . dap-auto-configure-mode)
  (dap-stopped . (lambda (arg) (dap-hydra)))
  (python-mode . (lambda ()
                   (require 'dap-python)
                   (setq dap-python-debugger 'debugpy)))
  ((c-mode c++-mode objc-mode swift-mode) . (lambda ()
					      (require 'dap-cpptools)
					      (dap-cpptools-setup)))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions tooltip)))

(provide 'zw-dap-mode)
