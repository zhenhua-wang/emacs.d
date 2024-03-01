;; -*- lexical-binding: t -*-

(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(push "~/.emacs.d/module" load-path)
(push "~/.emacs.d/experimental" load-path)

;; load module
(require 'zw-package)
(require 'zw-startup)
(require 'zw-base)
(require 'zw-mode-line)
(require 'zw-completion)
