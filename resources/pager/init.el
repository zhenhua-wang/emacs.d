;; speedup
(setq load-prefer-newer noninteractive
      frame-inhibit-implied-resize t
      package-enable-at-startup nil
      gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.6
      file-name-handler-alist nil
      warning-minimum-level :error)

;; user path
(setq user-emacs-directory (expand-file-name "~/.cache/emacs/"))
(push "~/.emacs.d/module" load-path)

;; appearance
(push '(menu-bar-lines . 0)   default-frame-alist)
(load-theme 'tango-dark t)

;; keybinding
(require 'zw-package)
(use-package kkp
  :commands (kkp-enable-in-terminal))
(use-package clipetty
  :bind ("C-c" . kill-ring-save))
(defun zw/isearch-yank-kill (event)
  (interactive "e")
  (unless isearch-mode (isearch-mode t))
  (isearch-yank-string (nth 1 event)))
(define-key global-map [xterm-paste] 'zw/isearch-yank-kill)
(bind-keys
 ("<escape>" . keyboard-quit)
 ("s-q" . kill-emacs)
 ("s-f" . isearch-forward)
 ("C-d" . isearch-delete-char)
 :map isearch-mode-map
 ("s-f" . isearch-repeat-forward)
 ("s-v" . isearch-yank-kill)
 ("<down>" . isearch-repeat-forward)
 ("<up>" . isearch-repeat-backward)
 ("<right>" . isearch-repeat-forward)
 ("<left>" . isearch-repeat-backward))
(advice-add 'self-insert-command :override
            (lambda (N &optional C)
              (unless isearch-mode (isearch-mode t))
              (isearch-printing-char C N)))

;; pager
(setq isearch-lazy-count t
      lazy-count-prefix-format "%s/%s "
      isearch-wrap-pause 'no
      visible-bell t)
(add-hook 'find-file-hook (lambda ()
                            ;; restore
                            (setq gc-cons-threshold 16777216
                                  gc-cons-percentage 0.1)
                            ;; pager
                            (read-only-mode 1)
                            (global-clipetty-mode 1)
                            (hl-line-mode 1)
                            (kkp-status)
                            (kkp-enable-in-terminal)
                            (setq-local mode-line-format nil)))
