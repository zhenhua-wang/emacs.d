;;; -*- lexical-binding: t -*-

;; speed up
(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      package-enable-at-startup nil)

;; user interface
(menu-bar-mode 0)
(setq-default mode-line-format nil)

;; user directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; native compilation
(setq native-comp-speed 2
      native-comp-async-query-on-exit t
      native-comp-jit-compilation nil
      native-comp-async-report-warnings-errors nil)

;; load theme
(load-theme 'misterioso t)
