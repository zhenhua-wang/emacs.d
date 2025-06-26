;; -*- lexical-binding: t -*-

(use-package eat
  :vc (:url "https://codeberg.org/akib/emacs-eat.git")
  :bind ((:map eat-mode-map
               ("s-e" . quit-window)
               ("s-E" . quit-window)
               ("s-S-e" . quit-window)
               ("s-t" . zw/eat-toggle-emacs-mode))
         (:map eat-semi-char-mode-map
               ("s-v" . eat-yank)
               ("s-z" . zw/eat-undo)))
  :hook
  ((eshell-load . eat-eshell-mode)
   (eshell-load . eat-eshell-visual-command-mode)
   (eat-mode . zw/global-hl-line-disable)
   (eat-mode . zw/eat-modeline-format)
   (eat-exec . zw/eat-setup))
  :custom
  (zw/term-function 'eat)
  (eat-kill-buffer-on-exit t)
  (eat-query-before-killing-running-terminal t)
  (eat-semi-char-non-bound-keys
   '([?\C-x] [?\C-\\] [?\C-q] [?\C-g] [?\C-h] [?\e ?\C-c] [?\C-u]
     [?\e ?x] [?\e ?:] [?\e ?!] [?\e ?&]))
  (eat-shell-prompt-annotation-success-margin-indicator (format "%s " (nerd-icons-faicon "nf-fa-check_circle")))
  (eat-shell-prompt-annotation-failure-margin-indicator (format "%s " (nerd-icons-faicon "nf-fa-times_circle")))
  (eat-shell-prompt-annotation-running-margin-indicator (format "%s " (nerd-icons-faicon "nf-fa-play_circle_o")))
  :config
  (defun zw/eat-setup (&rest args)
    (unless (file-remote-p default-directory)
      (let ((shell-type (if (string-match-p "zsh" eat-shell) "zsh" "bash")))
        (eat--send-string
         (eat-term-parameter eat-terminal 'eat--process)
         (format "source $EAT_SHELL_INTEGRATION_DIR/%s&&clear\n" shell-type)))))
  (defun zw/eat-toggle-emacs-mode ()
    (interactive)
    (if eat--semi-char-mode
        (eat-emacs-mode)
      (eat-semi-char-mode)))
  (defun zw/eat-undo ()
    (interactive)
    (eat--send-input nil (kbd "C-_")))
  (defun zw/eat-modeline-buffername ()
    (propertize
     (concat " " eat-buffer-name
             (unless eat--semi-char-mode
               (format " (%s) " (cond
                                 (eat--line-mode "Line")
                                 (eat--char-mode "Char")
                                 (t "Emacs")))))
     'face (zw/modeline-set-face 'zw/modeline-buffer-name-active
                                 'zw/modeline-default-inactive)))
  (defun zw/eat-modeline-format ()
    (setq-local
     mode-line-format
     (list
      "%e"
      '(:eval (zw/modeline-bar))
      ;; left
      '(:eval (zw/modeline-remote))
      '(:eval (zw/eat-modeline-buffername))
      (if (display-graphic-p) "" zw/modeline-separator)
      '(:eval (zw/modeline-text-scale))
      '(:eval (zw/modeline-read-only))
      '(:eval (zw/modeline-line-column))
      '(:eval (zw/modeline-mark-count))
      '(:eval (zw/modeline-kmacro-recording))
      ;; right
      '(:eval (zw/modeline-middle-space (zw/modeline-rhs)))
      '(:eval (zw/modeline-rhs))))))

(provide 'zw-eat)
