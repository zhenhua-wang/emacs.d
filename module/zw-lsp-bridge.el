;; -*- lexical-binding: t -*-

(use-package lsp-bridge
  :straight (lsp-bridge :host github :repo "manateelazycat/lsp-bridge"
                        :files ("*"))
  :commands (lsp-bridge-mode)
  :hook
  (python-mode . lsp-bridge-mode)
  ((c++-mode c-mode) . lsp-bridge-mode)
  (ess-r-mode . lsp-bridge-mode)
  (lsp-bridge-mode . (lambda () (company-mode -1)))
  :bind ((:map lsp-bridge-mode-map
               ("M-<tab>" . (lambda () (interactive) (acm-update)))
               ("s-d" . acm-doc-toggle)
               ("s-n" . acm-doc-scroll-up)
               ("s-p" . acm-doc-scroll-down))
         (:map acm-mode-map
               ("M->" . acm-select-last)
               ("M-<" . acm-select-first)))
  :config
  (setq acm-enable-doc nil)
  ;; send polymode content to lsp-bridge
  (defun zw-polymode-lsp-buffer-content (orig-fun &rest arguments)
    (if (and polymode-mode pm/polymode)
        (pm--lsp-text)
      (funcall orig-fun arguments)))
  (advice-add #'lsp-bridge--get-buffer-content-func
              :around #'zw-polymode-lsp-buffer-content))

(provide 'zw-lsp-bridge)
