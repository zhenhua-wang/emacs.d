;;; init.el --- ZW Configuration.	-*- lexical-binding: t -*-
;;; Commentary:

;;
;; ZW Emacs
;;

;;; Code:

(setq auto-mode-case-fold nil
      initial-scratch-message nil
      highlight-nonselected-windows nil
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      redisplay-skip-fontification-on-input t)

(setq-default inhibit-startup-screen t
	      cursor-in-non-selected-windows nil
              inhibit-redisplay t
              inhibit-message t)

;; restore messages after init
(add-hook 'window-setup-hook
          (lambda ()
            (setq-default inhibit-redisplay nil
                          inhibit-message nil)
            (redisplay)))

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

;; native-comp settings
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (progn
    (setq native-comp-async-report-warnings-errors nil
          comp-deferred-compilation nil)
    (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory))))

;; load init
(pcase system-type
  ('windows-nt (org-babel-load-file "~/.emacs.d/emacs-windows.org"))
  (_ (org-babel-load-file "~/.emacs.d/emacs.org")))
;; (org-babel-load-file "~/.emacs.d/emacs-plain-config.org")
