;; -*- lexical-binding: t -*-

(setq zw/theme-init-p nil)

(use-package auto-dark
  :hook ((after-init . auto-dark-mode)
         (auto-dark-dark-mode . zw/theme-load-ui)
         (auto-dark-light-mode . zw/theme-load-ui))
  :config
  (setq auto-dark-themes '((adwaita-dark) (doom-one-light))))

(provide 'zw-theme-auto)
