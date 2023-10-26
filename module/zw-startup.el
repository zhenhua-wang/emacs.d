;; -*- lexical-binding: t -*-

;; * Garbage collection
(use-package gcmh
  :hook
  (after-init . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold (* 16 1024 1024)))

;; * Keep .emacs.d Clean
;; Use no-littering to automatically set common paths to the new user-emacs-directory
(use-package no-littering)

;; Keep customization settings in a temporary file (thanks Ambrevar!)
(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                          temporary-file-directory)))
(load custom-file t)

;; * benchmark
(use-package benchmark-init
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; * Exec-path-from-shell
(use-package exec-path-from-shell
  :if (or (eq system-type 'darwin)
          (eq system-type 'gnu/linux)
          (daemonp))
  :hook
  (after-init . exec-path-from-shell-initialize)
  :init
  (setq exec-path-from-shell-check-startup-files nil))

;; * Provide
(provide 'zw-startup)
