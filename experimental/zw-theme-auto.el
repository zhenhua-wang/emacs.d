;; -*- lexical-binding: t -*-

(setq zw/theme-init-p nil)

(use-package auto-dark
  :hook ((after-init . auto-dark-mode)
         (auto-dark-dark-mode . zw/theme-load-ui)
         (auto-dark-light-mode . zw/theme-load-ui))
  :config
  (setq auto-dark-themes '((adwaita-dark) (doom-one-light)))
  :init
  (defun advice/auto-dark--enable-themes (auto-dark--enable-themes &rest args)
    (let ((custom-safe-themes t))
      (apply auto-dark--enable-themes args)))
  (advice-add 'auto-dark--enable-themes :around 'advice/auto-dark--enable-themes))

(provide 'zw-theme-auto)
