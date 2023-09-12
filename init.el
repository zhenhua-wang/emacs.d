;;; init.el --- ZW Configuration.	-*- lexical-binding: t -*-
;;; Commentary:

;;
;; ZW Emacs
;;

;;; Code:

(setq auto-mode-case-fold nil
      inhibit-startup-screen t
      initial-scratch-message nil)

;; Unset file-name-handler-alist temporarily and restore it later
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
(let ((zw/test-config nil))
  (if zw/test-config
      (org-babel-load-file "~/.emacs.d/emacs-minimal.org")
    (pcase system-type
      ('windows-nt (org-babel-load-file "~/.emacs.d/emacs-windows.org"))
      (_ (org-babel-load-file "~/.emacs.d/emacs.org")))))

;; clear echo area
(message nil)
