;; -*- lexical-binding: t -*-

;; * Undo
(use-package undo-fu
  :bind (("s-z" . undo-fu-only-undo)
         ("s-Z" . undo-fu-only-redo)))

;; * Auto save
(use-package super-save
  :diminish
  :hook (after-init . super-save-mode)
  :config (setq super-save-auto-save-when-idle t))

;; * Sudo edit
(use-package sudo-edit
  :commands (sudo-edit))

;; * Snippets
(use-package yasnippet
  :hook (after-init . yas-global-mode)
  :init (setq yas-snippet-dirs '("~/.emacs.d/yasnippet")))

;; * Visual regexp
(use-package visual-regexp
  :bind (("C-c r" . vr/replace)
         ("C-c q" . vr/query-replace)))

;; * Provide
(provide 'zw-editor)
