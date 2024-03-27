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
  (with-eval-after-load "zw-package"
    ;; full-featured keybindings
    (use-package kkp
      :init (setq kkp-terminal-query-timeout 1)
      :bind (:map global-map
                  ("s-S-z" . undo-fu-only-redo)
                  ("s-S-s" . write-file)
                  ("s-S-u" . winner-redo)
                  ("s-S-b" . zw/right-side-window-toggle)
                  ("s-S-p" . zw/repl-run-in-path)
                  ("s-S-g" . magit-status))
      :hook (after-init . global-kkp-mode))
    ;; copy and paste
    (use-package clipetty
      :hook (after-init . global-clipetty-mode))))

(provide 'zw-compat)
