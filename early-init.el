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
      package-enable-at-startup nil
      auto-mode-case-fold nil
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      redisplay-skip-fontification-on-input t
      ;; startup
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t
      initial-scratch-message nil)

;; user interface
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated . t)      default-frame-alist)
(setq-default mode-line-format nil)

;; user directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; native compilation
(setq native-comp-speed 2
      native-comp-async-query-on-exit t
      ;; native-comp-jit-compilation nil
      native-comp-async-report-warnings-errors nil)

;; load frame parameters
(setq zw/frame-parameters
      (expand-file-name "zw-frame-parameters.el" user-emacs-directory))
(when (file-exists-p zw/frame-parameters)
  (load zw/frame-parameters))
