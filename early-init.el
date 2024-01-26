;;; early-init.el --- Early initialization. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Emacs 27 introduces early-init.el, which is run before init.el,
;; before package and UI initialization happens.
;;

;;; Code:

;; speed up start-up
(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      package-enable-at-startup nil)

;; set UI
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
;; (push '(fullscreen . maximized) default-frame-alist)
;; (push '(undecorated . t) default-frame-alist)

;; set user directory
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))

;; native-comp settings
(when (and (featurep 'native-comp-available-p)
           (native-comp-available-p))
  (startup-redirect-eln-cache (expand-file-name  "var/eln-cache/" user-emacs-directory))
  (setq-default native-comp-speed 2
                native-comp-async-query-on-exit t
                native-comp-jit-compilation nil
                native-comp-async-report-warnings-errors nil))
