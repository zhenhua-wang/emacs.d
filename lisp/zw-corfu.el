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
  :bind (("M-<tab>" . completion-at-point)
         (:map corfu-map
               ("TAB" . corfu-insert)
               ([tab] . corfu-insert)
               ([escape] . corfu-quit)
               ([return] . corfu-insert)
               ("SPC" . corfu-insert-separator)))
  :init
  (setq corfu-cycle t
        corfu-auto t
        corfu-auto-delay 0
        corfu-auto-prefix 1
        corfu-preselect-first t
        corfu-quit-no-match t
        corfu-on-exact-match 'insert
        corfu-preview-current nil
        corfu-echo-documentation nil
        corfu-scroll-margin 0
        corfu-count 12
        corfu-min-width 40
        corfu-max-width 80
        corfu-bar-width 1
        corfu-excluded-modes '(eshell-mode-hook shell-mode-hook))
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  :config
  ;; set icon for corfu
  (require 'kind-all-the-icons)
  (add-to-list 'corfu-margin-formatters
               #'kind-all-the-icons-margin-formatter)
  (set-face-attribute 'corfu-default nil
                      :foreground (face-foreground 'tooltip)
                      :background (face-background 'tooltip)
                      :inherit 'fixed-pitch)
  (set-face-attribute 'corfu-current nil
                      :foreground (face-foreground 'warning)
                      :underline (face-foreground 'warning)
                      :background (face-background 'warning)
                      :weight 'bold)
  (set-face-attribute 'corfu-bar nil
                      :background (face-foreground font-lock-comment-face)))

;; corfu-doc
(use-package corfu-doc
  :hook
  (corfu-mode . corfu-doc-mode)
  :bind ((:map corfu-map
               ("s-p" . corfu-doc-scroll-down)
               ("s-n" . corfu-doc-scroll-up)
               ("s-d" . corfu-doc-toggle)))
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
