;; -*- lexical-binding: t -*-

(setq-default auto-mode-case-fold nil
              inhibit-startup-screen t
              initial-scratch-message nil
              ;; speed up emacs
              bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right
              bidi-inhibit-bpa t
              idle-update-delay 1.0
              inhibit-compacting-font-caches t
              redisplay-skip-fontification-on-input t)

;; Unset file-name-handler-alist temporarily and restore it later
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;; Load path for manually installed packages
(push "~/.emacs.d/module" load-path)

;; load module
(require 'zw-package)
(require 'zw-startup)
(require 'zw-base)
(require 'zw-modeline)
(require 'zw-completion)
