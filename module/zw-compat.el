;; -*- lexical-binding: t -*-

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
  (use-package kkp
    :config (global-kkp-mode +1))

  (bind-keys :map global-map
             ("s-S-s" . write-file)
             ("s-S-u" . winner-redo)
             ("s-S-b" . zw/right-side-window-toggle)
             ("s-S-p" . zw/repl-run-in-path)
             ("s-S-g" . magit-status)))

(provide 'zw-compat)
