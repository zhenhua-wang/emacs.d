;; -*- lexical-binding: t -*-

;;; code

(use-package dap-mode
  :hook
  (after-init . dap-auto-configure-mode)
  (dap-stopped . (lambda (arg) (debug-hydra)))
  (dap-terminated . (lambda (args) (debug-hydra/nil)))
  (python-mode . (lambda () (require 'dap-python)))
  ((c-mode c++-mode objc-mode swift-mode) . (lambda ()
					      (require 'dap-cpptools)
					      (dap-cpptools-setup)))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls)))

(provide 'zw-dap-mode)
