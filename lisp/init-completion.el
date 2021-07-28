;;;;;;;;;;;;;;;;;;;;;;;;;;;;; completion ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-active-map (kbd "M-h") 'company-show-doc-buffer)
  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map (kbd "M-,") 'company-select-next)
  (define-key company-active-map (kbd "M-k") 'company-select-previous)
  (define-key company-active-map [return] nil)
  (define-key company-active-map [tab] 'company-complete-common)
  (define-key company-active-map (kbd "TAB") 'company-complete-common)
  (define-key company-active-map (kbd "M-TAB") 'company-complete-selection)
  (setq company-selection-wrap-around t
	company-tooltip-align-annotations t
	company-idle-delay 0.36
	company-minimum-prefix-length 2
	company-tooltip-limit 10))

(use-package company-box
  :hook (company-mode . company-box-mode))

(use-package company-prescient
  :ensure t
  :after company
  :config
  (company-prescient-mode 1))

(use-package company-jedi :ensure t)

(provide 'init-completion)
