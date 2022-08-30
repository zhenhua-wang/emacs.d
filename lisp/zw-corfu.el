;; zw-corfu.el --- Initialize corfu configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Corfu Auto-completion configurations.
;;

;;; Code:

;; completion setting
(setq completion-cycle-threshold nil)
(setq tab-always-indent t)
(setq completions-detailed t)
(setq completion-ignore-case t)
;; since cape-dabbrev cannot replace case, I will set it to nil for now.
(use-package dabbrev
  :config
  (setq dabbrev-case-fold-search nil
        dabbrev-case-replace t
        dabbrev-ignored-buffer-regexps
        '("\\.\\(?:pdf\\|jpe?g\\|png\\)\\'")))

;; corfu
(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  (corfu-mode . yas-minor-mode)
  ;; force corfu start in ess-r-mode
  (ess-r-mode . corfu-mode)
  :bind ((:map corfu-map
               ("TAB" . corfu-insert)
               ([tab] . corfu-insert)
               ([escape] . corfu-quit)
               ([return] . corfu-insert)
               ("SPC" . corfu-insert-separator)))
  :init
  ;; fast prefix filtering
  (defun orderless-fast-dispatch (word index total)
    (and (= index 0) (= total 1) (length< word 4)
         `(orderless-regexp . ,(concat "^" (regexp-quote word)))))

  (orderless-define-completion-style orderless-fast
    (orderless-dispatch '(orderless-fast-dispatch))
    (orderless-matching-styles '(orderless-literal orderless-regexp)))

  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1
        completion-styles '(orderless-fast)
        corfu-preselect-first t
        corfu-quit-no-match t
        corfu-on-exact-match 'insert
        corfu-preview-current nil
        corfu-echo-documentation nil
        corfu-scroll-margin 5
        corfu-count 10
        corfu-min-width 20
        corfu-max-width 80
        corfu-bar-width 1)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; disable corfu auto in following modes
  (dolist (hook '(inferior-ess-r-mode-hook eshell-mode-hook shell-mode-hook))
    (add-hook hook (lambda () (setq-local corfu-auto nil))))
  :config
  ;; set icon for corfu
  (require 'kind-all-the-icons)
  (add-to-list 'corfu-margin-formatters
               #'kind-all-the-icons-margin-formatter)
  (defun zw/corfu-theme ()
    ;; FIXME: Popup size is wrong https://github.com/minad/corfu/issues/149
    (set-face-attribute 'corfu-default nil
                        :foreground (face-foreground 'tooltip)
                        :background (face-background 'tooltip)
                        :inherit 'fixed-pitch)
    (set-face-attribute 'corfu-current nil
                        :foreground (face-foreground 'warning)
                        :underline (face-foreground 'warning)
                        :background (face-background 'warning)
                        :weight 'bold)
    (with-eval-after-load 'doom-modeline
      (set-face-attribute 'corfu-border nil
                          :background (zw/get-face-bg-recur 'doom-modeline-bar)))
    (set-face-attribute 'corfu-bar nil
                        :background (face-foreground font-lock-comment-face)))
  (zw/corfu-theme))

;; corfu-doc
(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode)
  :bind ((:map corfu-map
               ("M-p" . corfu-doc-scroll-down)
               ("M-n" . corfu-doc-scroll-up)
               ("M-d" . corfu-doc-toggle)))
  :config
  (setq corfu-doc-auto nil
        corfu-doc-display-within-parent-frame t))

;; cape
(use-package cape
  :after corfu
  :bind (("C-c c f" . cape-file)
         ("C-c c d" . cape-dabbrev))
  :hook
  (after-change-major-mode . add-cape-completion)
  :init
  (defun add-cape-completion ()
    ;; Add `completion-at-point-functions', used by `completion-at-point'.
    (add-to-list 'completion-at-point-functions #'cape-file)
    (add-to-list 'completion-at-point-functions #'cape-dabbrev t)))

(provide 'zw-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zw-corfu.el ends here
