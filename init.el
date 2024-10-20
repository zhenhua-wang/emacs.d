;; -*- lexical-binding: t -*-

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
(require 'zw-compat)
(require 'zw-startup)
(require 'zw-base)
;; appearance
(require 'zw-theme)
(require 'zw-appearance)
(require 'zw-mode-line)
(require 'zw-tab-bar)
(require 'zw-tab-line)
;; tools
(require 'zw-tools)
(require 'zw-editor)
(require 'zw-completion)
;; development
(require 'zw-ide)
(require 'zw-lang)
;; document
(require 'zw-document)

;; user config
(setq zw/user-config
      (expand-file-name "zw-user-config.el" user-emacs-directory))
(when (not (file-exists-p zw/user-config))
  (copy-file "~/.emacs.d/resources/scripts/zw-user-config.el" zw/user-config))
(load zw/user-config)
(defun zw/open-user-config ()
  "Open user configuration file."
  (interactive)
  (find-file zw/user-config))
