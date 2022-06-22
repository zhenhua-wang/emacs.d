;; zw-vertico.el --- Initialize ivy configurations.	-*- lexical-binding: t -*-

;;; Commentary:
;;
;; Vertico Mini-buffer completion configurations.
;;

;;; Code:

;; Enable vertico
(use-package vertico
  :straight (:files (:defaults "extensions/*")
		    :includes (vertico-buffer
			       vertico-directory
			       vertico-flat
			       vertico-indexed
			       vertico-mouse
			       vertico-quick
			       vertico-repeat
			       vertico-reverse))
  :hook
  (after-init . vertico-mode)
  ;; Tidy shadowed file names
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  ;; More convenient directory navigation commands
  :bind (:map vertico-map
              ("RET" . vertico-directory-enter)
              ("DEL" . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word))
  :init
  (setq vertico-resize t
	vertico-scroll-margin 0
	vertico-count 15
	vertico-cycle t)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)

  ;; extensions
  (use-package vertico-directory :after vertico :straight nil))

;; Enable richer annotations using the Marginalia package
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :hook (vertico-mode . marginalia-mode))

;; add icons to vertico
(use-package all-the-icons-completion
  :after (marginalia all-the-icons)
  :hook
  (marginalia-mode . all-the-icons-completion-marginalia-setup)
  (marginalia-mode . all-the-icons-completion-mode)
  :init
  (setq marginalia-max-relative-age 0
	marginalia-align 'center))

;; Example configuration for Consult
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)                ;; orig. switch-to-buffer
	 ("C-x C-d" . consult-dir)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)                ;; orig. yank-pop
         ("<help> a" . consult-apropos)            ;; orig. apropos-command
	 ("s-f" . consult-line)                ;; orig. yank-pop
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)             ;; orig. goto-line
         ("M-g o" . consult-outline)               ;; Alternative: consult-org-heading
         ("M-g i" . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
	 ("M-s b" . consult-bibtex-open-pdf)
	 ("M-s y" . consult-yasnippet)
	 ("M-s c" . consult-flycheck)
	 :map isearch-mode-map
         ("M-s" . consult-isearch-history)
	 :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir))

  :init
  (setq consult-preview-key nil)
  :config
  (consult-customize
   consult-theme
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file)

  ;; Optionally configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "<" ;; (kbd "C-+")
	consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)

  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))

  ;; Preview while using consult-theme
  (consult-customize consult-theme :preview-key '(:debounce 0.5 any))
  ;; Preview immediately on M-., on up/down after 0.5s, on any other key after 1s
  (consult-customize consult-theme
                     :preview-key
                     (list (kbd "M-.")
                           :debounce 0.5 (kbd "<up>") (kbd "<down>")
                           :debounce 1 'any))

  ;; show org files in switch-buffer
  (defvar org-source
    (list :name     "Org Buffer"
          :category 'buffer
          :narrow   ?o
          :face     'consult-buffer
          :history  'buffer-name-history
          :state    #'consult--buffer-state
          :new
          (lambda (name)
            (with-current-buffer (get-buffer-create name)
              (insert "#+title: " name "\n\n")
              (org-mode)
              (consult--buffer-action (current-buffer))))
          :items
          (lambda ()
            (mapcar #'buffer-name
                    (seq-filter
                     (lambda (x)
                       (eq (buffer-local-value 'major-mode x) 'org-mode))
                     (buffer-list))))))
  (add-to-list 'consult-buffer-sources 'org-source)

  ;; custom consult packages
  (use-package consult-yasnippet :after consult)
  (use-package consult-bibtex :after consult
    :straight `(consult-bibtex :host github :repo "mohkale/consult-bibtex"))
  (use-package consult-dir :after consult)
  (use-package  consult-lsp :after consult
    :config
    (define-key lsp-mode-map [remap xref-find-apropos] #'consult-lsp-symbols))
  (use-package consult-flycheck
		:after (consult flycheck)))

(use-package embark
  :bind
  (("C-," . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(provide 'zw-vertico)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zw-vertico.el ends here
