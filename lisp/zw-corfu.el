;; zw-corfu.el --- Initialize corfu configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Corfu Auto-completion configurations.
;;

;;; Code:

(use-package corfu
  :hook
  (after-init . global-corfu-mode)
  ;; hook to lsp mode
  (lsp-completion-mode . zw/lsp-mode-setup-completion)
  :bind
  (:map corfu-map
	("TAB" . corfu-insert)
        ([tab] . corfu-insert)
        ([escape] . corfu-quit)
        ([return] . corfu-insert)
        ("M-d" . corfu-show-documentation)
        ("M-l" . corfu-show-location)
	("SPC" . corfu-insert-separator))
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
	corfu-scroll-margin 5
	corfu-min-width 20
	corfu-max-width 80)
  (defun corfu-enable-in-minibuffer ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (where-is-internal #'completion-at-point (list (current-local-map)))
      (corfu-mode 1)))
  (add-hook 'minibuffer-setup-hook #'corfu-enable-in-minibuffer)
  ;; disable corfu auto in following modes
  (dolist (hook '(inferior-ess-r-mode-hook))
    (add-hook hook (lambda () (setq-local corfu-auto nil))))
  ;; setup corfu in lsp mode
  (defun zw/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless))
    (setq lsp-completion-mode :none))
  :config
  (use-package dabbrev
    :custom
    ;; since cape-dabbrev cannot replace case, I will set it to nil for now.
    (dabbrev-case-fold-search nil)
    (dabbrev-case-replace t))

  (use-package kind-icon
    :after corfu
    :init
    (setq kind-icon-use-icons t
	  kind-icon-default-face 'corfu-default)
    (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

  (use-package corfu-doc
    :hook
    (corfu-mode . corfu-doc-mode)
    :bind
    (:map corfu-map
          ("M-p" . corfu-doc-scroll-down)
          ("M-n" . corfu-doc-scroll-up)))


  ;; orderless
  (use-package orderless
    ;; :after corfu
    :config
    (setq completion-styles '(orderless partial-completion basic)
          completion-category-defaults nil
          completion-category-overrides nil))

  ;; Add extensions
  (use-package cape
    :after corfu
    :bind
    ("C-c f" . cape-file)
    ("C-c d" . cape-dabbrev)
    :hook
    (after-change-major-mode . add-cape-completion)
    :init
    (setq cape-dabbrev-min-length 1)
    (defun add-cape-completion ()
      ;; Add `completion-at-point-functions', used by `completion-at-point'.
      (add-to-list 'completion-at-point-functions #'cape-file)
      (add-to-list 'completion-at-point-functions #'cape-dabbrev t))))

(provide 'zw-corfu)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zw-company.el ends here
