;; -*- lexical-binding: t -*-

;; * Mouse mode
(xterm-mouse-mode 1)
(menu-bar-mode 0)
(define-key global-map (kbd "<menu-bar> <mouse-1>") nil)

;; * CUA mode
(cua-mode 1)
(define-key cua-global-keymap (kbd "C-<return>") nil)
(define-key cua--cua-keys-keymap (kbd "C-v") nil)
(define-key cua--cua-keys-keymap (kbd "M-v") nil)

;; * Keyboard
(use-package kkp
  :hook (after-init . zw/kkp-enable)
  :init
  (setq kkp-terminal-query-timeout 1)
  (cl-defun zw/kkp-restart (&optional (terminal (kkp--selected-terminal)))
    (interactive)
    (kkp--terminal-teardown (kkp--selected-terminal))
    (set-terminal-parameter terminal 'kkp--setup-started t)
    (kkp--query-terminal-async "?u\e[c"
                               '(("\e[?" . kkp--terminal-setup)) terminal))
  (defun zw/xterm-paste (event)
    (interactive "e")
    (when (and (use-region-p)
               delete-selection-mode)
      (delete-region (region-beginning) (region-end)))
    (call-interactively 'xterm-paste nil (vector event)))
  (defun zw/kkp-enable ()
    (global-kkp-mode 1)
    (define-key global-map [xterm-paste] #'zw/xterm-paste)))

;; * Clipborad
;; fix for server
(setenv "SSH_TTY")

(use-package clipetty
  :hook (after-init . global-clipetty-mode))

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
             ("<f12>" . nil)))

;; * Provide
(provide 'zw-tty)
