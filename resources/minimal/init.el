;; -*- lexical-binding: t -*-

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(push "~/.emacs.d/module" load-path)
(push "~/.emacs.d/experimental" load-path)

;; load module
(require 'zw-package)
(require 'zw-startup)
(require 'zw-base)

;; minimal config
(setq zw/minimal-config
      (expand-file-name "zw-minimal-config.el" user-emacs-directory))
(when (not (file-exists-p zw/minimal-config))
  (with-temp-buffer (write-file zw/minimal-config)))
(load zw/minimal-config)
(defun zw/open-minimal-config ()
  (interactive)
  (find-file zw/minimal-config))
