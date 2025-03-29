;; -*- lexical-binding: t -*-

(use-package auto-dark
  :hook (after-init . auto-dark-mode)
  :config
  (setq auto-dark-themes '((adwaita-dark) (doom-one-light)))
  (add-hook 'auto-dark-dark-mode-hook
            (lambda ()
              (zw/theme-load-ui)
              (zw/tab-line-set-face)
              (tab-line-format)))
  (add-hook 'auto-dark-light-mode-hook
            (lambda ()
              (zw/theme-load-ui)
              (zw/tab-line-set-face)
              (tab-line-format))))

(provide 'zw-theme-extra)
