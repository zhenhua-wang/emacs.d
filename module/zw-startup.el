;; -*- lexical-binding: t -*-

;; * Garbage collection
(use-package gcmh
  :hook
  (emacs-startup . gcmh-mode)
  :init
  (setq gcmh-idle-delay 'auto
        gcmh-auto-idle-delay-factor 10
        gcmh-high-cons-threshold #x1000000))

;; * Cache
(use-package no-littering)

(setq custom-file
      (if (boundp 'server-socket-dir)
          (expand-file-name "custom.el" server-socket-dir)
        (expand-file-name (format "emacs-custom-%s.el" (user-uid))
                          temporary-file-directory)))
(load custom-file t)

;; * Execute path from shell
(use-package exec-path-from-shell
  :if (or (eq system-type 'darwin)
          ;; (eq system-type 'gnu/linux)
          (daemonp))
  :init
  (setq exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;; * Provide
(provide 'zw-startup)
