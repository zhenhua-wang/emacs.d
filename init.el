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

;; disable initial message
(setq-default inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; load init
(org-babel-load-file "~/.emacs.d/emacs.org")
;; (org-babel-load-file "~/.emacs.d/emacs-plain-config.org")
