;; -*- lexical-binding: t -*-

(when (< emacs-major-version 27)
  (load "~/.emacs.d/early-init.el"))

(unless (fboundp 'string-pixel-width)
  (defun string-pixel-width (string)
    (if string
        (string-width string)
      0)))

(unless (fboundp 'mode-line-window-selected-p)
  (defun mode-line-window-selected-p ()
    (let ((window (selected-window)))
      (or (eq window (old-selected-window))
	  (and (minibuffer-window-active-p (minibuffer-window))
	       (with-selected-window (minibuffer-window)
	         (eq window (minibuffer-selected-window))))))))

(unless (display-graphic-p)
  (with-eval-after-load "zw-package"
    (straight-use-package 'kkp)
    (use-package kkp
      :init (setq kkp-terminal-query-timeout 1)
      :config
      (bind-keys :map global-map
                 ("s-S-s" . write-file)
                 ("s-S-u" . winner-redo)
                 ("s-S-b" . zw/right-side-window-toggle)
                 ("s-S-p" . zw/repl-run-in-path)
                 ("s-S-g" . magit-status))
      (global-kkp-mode +1))))

(provide 'zw-compat)
