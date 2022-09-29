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
      frame-inhibit-implied-resize t)

;; set UI
(push '(menu-bar-lines . 0)   default-frame-alist)
(push '(tool-bar-lines . 0)   default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(fullscreen . maximized) default-frame-alist) ; maximized
(push '(undecorated . t) default-frame-alist)        ; remove title
