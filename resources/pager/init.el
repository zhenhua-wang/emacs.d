;; -*- lexical-binding: t; -*-

;; config
(setq gc-cons-percentage 0.6
      file-name-handler-alist nil
      warning-minimum-level :error
      auto-mode-case-fold nil
      inhibit-startup-screen t
      initial-scratch-message nil
      ;; speed up emacs
      idle-update-delay 1.0
      inhibit-compacting-font-caches t
      redisplay-skip-fontification-on-input t
      ;; optimize long file
      bidi-display-reordering nil
      bidi-inhibit-bpa t
      long-line-threshold 1000
      large-hscroll-threshold 1000
      syntax-wholeline-max 1000)

;; user path
(setq auto-save-list-file-prefix (expand-file-name "auto-save/sessions/" user-emacs-directory))
(push "~/.emacs.d/module" load-path)

;; keybinding
(require 'zw-package)
(use-package kkp
  :commands (kkp-enable-in-terminal))
(use-package clipetty
  :bind ("C-c" . kill-ring-save))
(use-package delsel
  :straight (:type built-in)
  :commands (minibuffer-keyboard-quit)
  :bind (:map minibuffer-mode-map
              ("<escape>" . minibuffer-keyboard-quit)))
(bind-keys
 ("<escape>" . keyboard-quit)
 ("s-q" . kill-emacs)
 ("s-f" . isearch-forward)
 ("s-z" . undo-only)
 ("s-S-z" . undo-redo)
 ("C-d" . backward-delete-char)
 :map isearch-mode-map
 ([remap isearch-delete-char] . isearch-del-char)
 ([xterm-paste] . isearch-xterm-paste)
 ("C-d" . isearch-del-char)
 ("s-f" . isearch-repeat-forward)
 ("s-v" . isearch-yank-kill)
 ("<down>" . isearch-repeat-forward)
 ("<up>" . isearch-repeat-backward)
 ("<right>" . isearch-repeat-forward)
 ("<left>" . isearch-repeat-backward))
(advice-add 'self-insert-command :around
            (lambda (orig-fun N &optional C)
              (if (minibufferp)
                  (funcall orig-fun N C)
                (progn
                  (unless isearch-mode (isearch-mode t))
                  (isearch-printing-char C N)))))
(defun zw/pager-isearch-xterm-paste (event)
  (interactive "e")
  (if (minibufferp)
      (call-interactively 'xterm-paste)
    (when (eq (car-safe event) 'xterm-paste)
      (unless isearch-mode (isearch-mode t))
      (let ((pasted-text (nth 1 event)))
        (isearch-yank-string pasted-text)))))

;; pager
(setq isearch-lazy-count t
      isearch-wrap-pause 'no
      isearch-allow-motion t
      search-nonincremental-instead nil
      search-whitespace-regexp ".*?"
      lazy-count-prefix-format "%s/%s "
      visible-bell t
      hscroll-margin 1
      hscroll-step 1)
(add-hook 'find-file-hook (lambda ()
                            ;; pager
                            (require 'ansi-color)
                            (ansi-color-apply-on-region (point-min) (point-max))
                            (read-only-mode 1)
                            (hl-line-mode 1)
                            (auto-save-mode 0)
                            (toggle-truncate-lines 1)
                            (set-display-table-slot standard-display-table 'truncation 32)
                            (global-clipetty-mode 1)
                            (kkp-status)
                            (kkp-enable-in-terminal)
                            (define-key global-map [xterm-paste] 'zw/pager-isearch-xterm-paste)
                            ;; restore
                            (setq gc-cons-threshold 16777216
                                  gc-cons-percentage 0.1)))
