;; -*- lexical-binding: t -*-

(defcustom zw/theme-auto-enable t
  "Whether to enable automatic theme.

Possible values:
- 'gui  : Enable automatic theme only when in GUI.
- 'tui  : Enable automatic theme only when in TUI.
- t     : Always enable automatic theme.
- nil   : Disable automatic theme enabling."
  :type '(choice (const :tag "Enable for GUI" gui)
                 (const :tag "Enable for TUI" tui)
                 (const :tag "Enable" t)
                 (const :tag "Disable" nil)))

(defun zw/theme-auto-enable-p ()
  (pcase zw/theme-auto-enable
    ('gui (display-graphic-p))
    ('tui (not (display-graphic-p)))
    ('t t)
    (_ nil)))

(defun zw/theme-auto ()
  (when (zw/theme-auto-enable-p)
    (setq zw/theme-init-p nil)
    (use-package auto-dark
      :hook ((auto-dark-dark-mode . zw/theme-load-ui)
             (auto-dark-light-mode . zw/theme-load-ui))
      :init
      (setq auto-dark-themes '((adwaita-dark) (doom-one-light))
            custom-safe-themes t)
      (auto-dark-mode))))
(add-hook 'zw/theme-init-before-hook 'zw/theme-auto)

(provide 'zw-theme-auto)
