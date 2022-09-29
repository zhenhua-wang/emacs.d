;;; init.el --- ZW Configuration.	-*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:

;;
;; ZW Emacs
;;

;;; Code:

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum
      load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      initial-scratch-message nil)

(setq-default inhibit-startup-screen t
	      cursor-in-non-selected-windows nil
              inhibit-redisplay t
              inhibit-message t)
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

;; native-comp settings
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil)
    (setq comp-deferred-compilation t)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))
    (setq package-native-compile t)))

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

;; load init
(pcase system-type
  ('windows-nt (org-babel-load-file "~/.emacs.d/emacs-windows.org"))
  (_ (org-babel-load-file "~/.emacs.d/emacs.org")))
;; (org-babel-load-file "~/.emacs.d/emacs-plain-config.org")
