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
(load-theme 'misterioso t)

;; keybinding
(require 'zw-package)
(use-package kkp
  :commands (kkp-enable-in-terminal))
(use-package clipetty
  :bind ("C-c" . kill-ring-save))
(bind-keys
 ("<escape>" . keyboard-quit)
 ("RET" . kill-emacs)
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
      visible-bell t)
(add-hook 'find-file-hook (lambda ()
                            ;; restore
                            (setq gc-cons-threshold 16777216
                                  gc-cons-percentage 0.1)
                            ;; pager
                            (read-only-mode 1)
                            (hl-line-mode 1)
                            (global-clipetty-mode 1)
                            (kkp-status)
                            (kkp-enable-in-terminal)
                            (define-key global-map [xterm-paste] 'zw/pager-isearch-xterm-paste)
                            (setq-local mode-line-format nil)
                            (call-interactively 'isearch-forward)))
