;; -*- lexical-binding: t -*-

(setq-default auto-mode-case-fold nil
              inhibit-startup-screen t
              initial-scratch-message nil
              ;; speed up emacs
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

;; base
(require 'zw-package)
(require 'zw-startup)
(require 'zw-base)
;; appearance
(require 'zw-ui)
(require 'zw-theme)
(require 'zw-modeline)
;; tools
(require 'zw-tools)
(require 'zw-editor)
(require 'zw-completion)
(require 'zw-company)
;; development
(require 'zw-ide)
(require 'zw-lang)
;; document
(require 'zw-document)
(require 'zw-literate)
