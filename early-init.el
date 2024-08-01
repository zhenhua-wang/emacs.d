;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; speed up
(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      package-enable-at-startup nil)

;; use package
(setq use-package-always-defer t
      use-package-expand-minimally t)

;; user interface
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) initial-frame-alist)
(push '(undecorated . t) initial-frame-alist)
(setq-default mode-line-format nil)

;; user directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; native compilation
(setq native-comp-speed 2
      native-comp-async-query-on-exit t
      ;; native-comp-jit-compilation nil
      native-comp-async-report-warnings-errors nil)
