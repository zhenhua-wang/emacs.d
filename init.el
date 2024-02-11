;; -*- lexical-binding: t -*-

(setq-default auto-mode-case-fold nil
              inhibit-startup-screen t
              initial-scratch-message nil
              ;; speed up emacs
              idle-update-delay 1.0
              inhibit-compacting-font-caches t
              redisplay-skip-fontification-on-input t
              ;; optimize long file
              bidi-display-reordering nil
              bidi-inhibit-bpa t
              long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000)

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
(push "~/.emacs.d/experimental" load-path)

;; base
(require 'zw-package)
(require 'zw-startup)
(require 'zw-base)
;; appearance
(require 'zw-appearance)
(require 'zw-modeline)
(require 'zw-tab-bar)
(require 'zw-tab-line)
(require 'zw-theme)
;; tools
(require 'zw-tools)
(require 'zw-editor)
(require 'zw-completion)
;; development
(require 'zw-ide)
(require 'zw-lang)
;; document
(require 'zw-document)
(require 'zw-literate)

;; user config
(setq zw/user-config
      (expand-file-name "zw-user-config.el" user-emacs-directory))
(when (not (file-exists-p zw/user-config))
  (with-temp-buffer (write-file zw/user-config)))
(load zw/user-config)
(defun zw/open-user-config ()
  (interactive)
  (find-file zw/user-config))
