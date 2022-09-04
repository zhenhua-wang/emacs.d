;; -*- lexical-binding: t -*-

;;; code

(use-package dap-mode
  :hook
  (after-init . dap-auto-configure-mode)
  (dap-stopped . (lambda (arg) (dap-hydra)))
  (python-mode . (lambda () (require 'dap-python)))
  ((c-mode c++-mode objc-mode swift-mode) . (lambda ()
					      (require 'dap-cpptools)
					      (dap-cpptools-setup)))
  :config
  (setq dap-python-debugger 'debugpy))

(provide 'zw-dap-mode)
