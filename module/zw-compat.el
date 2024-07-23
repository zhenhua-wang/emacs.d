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
    ;; enable cua-mode to handle C-c
    (cua-mode 1)
    (define-key cua-global-keymap (kbd "C-<return>") nil)
    ;; full-featured keybindings
    (straight-use-package 'kkp)
    (use-package kkp
      :bind (:map global-map
                  ("s-S-z" . undo-redo)
                  ("s-S-s" . write-file)
                  ("s-S-u" . winner-redo)
                  ("s-S-b" . zw/right-side-window-toggle)
                  ("s-S-p" . zw/conda-env-activate)
                  ("s-S-g" . magit-status))
      :init
      (setq kkp-terminal-query-timeout 1)
      (global-kkp-mode 1))
    ;; copy and paste
    (defun zw/xterm-paste (event)
      (interactive "e")
      (when (and (use-region-p)
                 delete-selection-mode)
        (delete-region (region-beginning) (region-end)))
      (call-interactively 'xterm-paste))
    (define-key global-map [xterm-paste] #'zw/xterm-paste)
    (straight-use-package 'clipetty)
    (use-package clipetty
      :hook (after-init . global-clipetty-mode)))
  (with-eval-after-load "zw-theme"
    (defun zw/theme-compat ()
      (set-face-attribute 'tab-line nil :underline nil)
      (set-face-attribute 'header-line nil
                          :background (face-background 'mode-line)
                          :underline nil
                          :weight 'bold))
    (advice-add 'zw/theme-set-theme :after 'zw/theme-compat))
  (with-eval-after-load "zw-base"
    (bind-keys :map global-map
               ("<f12>" . nil))))

(provide 'zw-compat)
