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

;; Load path for manually installed packages
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Load path for customied themes
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))

;; straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
	 "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Install use-package
(straight-use-package 'use-package)
;; Configure use-package to use straight.el by default
(setq straight-use-package-by-default t)
;; disable checking at start-up
(setq straight-check-for-modifications '(watch-files find-when-checking))
(setq use-package-always-ensure t)
(setq use-package-verbose t)

;; load init
(pcase system-type
  ('windows-nt (org-babel-load-file "~/.emacs.d/emacs-windows.org"))
  (_ (progn (org-babel-load-file "~/.emacs.d/emacs.org")
            (org-babel-load-file "~/.emacs.d/emacs-development.org")
            (org-babel-load-file "~/.emacs.d/emacs-academic.org"))))
;; (org-babel-load-file "~/.emacs.d/emacs-plain-config.org")
