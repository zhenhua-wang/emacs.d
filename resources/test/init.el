;; -*- lexical-binding: t -*-

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(push "~/.emacs.d/module" load-path)
(push "~/.emacs.d/experimental" load-path)

;; load module
(require 'zw-package)
(require 'zw-startup)
(require 'zw-base)

;; test config
(setq zw/test-config
      (expand-file-name "zw-test-config.el" user-emacs-directory))
(when (not (file-exists-p zw/test-config))
  (with-temp-buffer (write-file zw/test-config)))
(load zw/test-config)
(defun zw/open-test-config ()
  (interactive)
  (find-file zw/test-config))
