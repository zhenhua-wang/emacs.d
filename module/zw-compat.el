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
    (straight-use-package 'kkp)
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
    (straight-use-package 'clipetty)
    (use-package clipetty
      :hook (after-init . global-clipetty-mode)))
  (with-eval-after-load "zw-theme"
    (defun zw/theme-compat ()
      (custom-set-faces
       `(tab-line ((t (:underline unspecified))))
       `(zw/modeline-highlight-foreground-active ((t (:foreground ,(face-foreground 'default) :bold t))))
       `(header-line ((t (:background ,(face-background 'mode-line) :bold t))))))
    (zw/theme-compat)
    (advice-add 'zw/theme-set-theme :after 'zw/theme-compat)))

(provide 'zw-compat)
