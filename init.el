;;; init.el --- ZW Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:

;;
;; ZW Emacs
;;

;;; Code:

;; Speed up startup
(setq auto-mode-case-fold nil)
(unless (or (daemonp) noninteractive init-file-debug)
  (let ((old-file-name-handler-alist file-name-handler-alist))
    (setq file-name-handler-alist nil)
    (add-hook 'emacs-startup-hook
              (lambda ()
                "Recover file name handlers."
                (setq file-name-handler-alist
                      (delete-dups (append file-name-handler-alist
                                           old-file-name-handler-alist)))))))

;; Set up the visible bell
(setq visible-bell t)

;; hide startup screen
(setq-default inhibit-startup-screen t
	      cursor-in-non-selected-windows nil)

;; hide initial message
(setq initial-scratch-message nil)

;; load init
(org-babel-load-file "~/.emacs.d/emacs.org")
;; (org-babel-load-file "~/.emacs.d/emacs-plain-config.org")
