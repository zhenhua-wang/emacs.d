;; -*- lexical-binding: t -*-

;; * Orederless
(use-package orderless
  :custom
  (completion-category-defaults nil)
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

;; * Vertico
(use-package vertico
  :hook
  (after-init . vertico-mode)
  (vertico-mode . vertico-multiform-mode)
  (vertico-mode . vertico-mouse-mode)
  ;; Tidy shadowed file names
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  ;; More convenient directory navigation commands
  :bind ((:map vertico-map
               ("RET" . vertico-directory-enter)
               ("M-RET" . vertico-exit-input)
               ("DEL" . vertico-directory-delete-char)
               ("M-DEL" . vertico-directory-delete-word)
               ("M-<backspace>" . vertico-directory-delete-word)))
  :init
  (setq vertico-resize nil
        vertico-scroll-margin 0
        vertico-count 12
        vertico-cycle t
        vertico-preselect 'directory)
  :config
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; use vertico as the interface for completion-at-point
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))
  ;; Sort directories before files (vertico-multiform-mode)
  (setq vertico-multiform-categories
        '((file (vertico-sort-function . sort-directories-first))))
  (defun sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (cl-remove-if-not (lambda (x) (string-suffix-p "/" x)) files)
           (cl-remove-if (lambda (x) (string-suffix-p "/" x)) files))))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :bind (:map vertico-multiform-map
              ("M-p" . nil))
  :config
  (defun zw/vertico-posframe-init (&rest args)
    "Initialize vertico-posframe UI."
    (setq vertico-posframe-border-width 20
          vertico-posframe-poshandler 'posframe-poshandler-frame-center
          vertico-posframe-parameters '((left-fringe . 8)
                                        (right-fringe . 8))
          vertico-posframe-width (floor (/ (frame-pixel-width) (frame-char-width) 1.5))))
  (defun zw/vertico-posframe-refresh ()
    "Refresh vertico-posframe."
    (interactive)
    (vertico-posframe-cleanup)
    (when (> (minibuffer-depth) 0)
      (vertico-posframe--show vertico-posframe--buffer 0)))
  (advice-add 'vertico-posframe--show :before 'zw/vertico-posframe-init)
  (add-hook 'zw/after-set-theme-hook 'zw/vertico-posframe-refresh)
  (add-hook 'server-after-make-frame-hook 'zw/vertico-posframe-refresh))

;; * Marginalia
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :hook (vertico-mode . marginalia-mode)
  :config
  ;; show mode on/off
  (defun marginalia-annotate-command (cand)
    (when-let ((sym (intern-soft cand)))
      (concat
       (when (boundp sym)
         (if (symbol-value sym)
             (propertize " [On]" 'face 'marginalia-on)
           (propertize " [Off]" 'face 'marginalia-off)))
       (marginalia-annotate-binding cand)
       (marginalia--documentation (marginalia--function-doc sym))))))

;; * Consult
(use-package consult
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ("C-c o" . consult-open-externally)
         ;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("s-f" . consult-line)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s g" . zw/consult-grep)
         ("M-s m" . consult-minor-mode-menu)
         ("M-s f" . consult-flymake)
         (:map isearch-mode-map
               ("M-s" . consult-isearch-history)))
  :init
  (setq consult-preview-key "M-."
        register-preview-delay 0.5
        register-preview-function #'consult-register-format
        xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)
  (advice-add #'register-preview :override #'consult-register-window)
  :config
  (consult-customize consult-theme :preview-key '(:debounce 0.2 any))
  ;; Optionally configure the narrowing key.
  (setq consult-narrow-key "<" ;; (kbd "C-+")
        consult-line-numbers-widen t
        consult-async-min-input 2
        consult-async-refresh-delay  0.15
        consult-async-input-throttle 0.2
        consult-async-input-debounce 0.1)
  ;; custom functions
  (defun zw/consult-line-multi ()
    (interactive)
    (consult-line-multi t))
  (defun zw/consult-grep ()
    (interactive)
    (if (executable-find "rg")
        (call-interactively 'consult-ripgrep)
      (call-interactively 'consult-grep)))
  (defun consult-open-externally (file)
    "Open FILE using system's default application."
    (interactive "fOpen externally: ")
    (if (and (eq system-type 'windows-nt)
             (fboundp 'w32-shell-execute))
        (w32-shell-execute "open" file)
      (call-process (pcase system-type
                      ('darwin "open")
                      ('cygwin "cygstart")
                      (_ "xdg-open"))
                    nil 0 nil
                    (expand-file-name file)))))

;; other consult packages
(use-package consult-yasnippet
  :bind (("M-s y" . consult-yasnippet)))
(use-package consult-dir
  :bind (("C-x C-d" . consult-dir)
         (:map minibuffer-local-completion-map
               ("C-x C-d" . consult-dir))))
(use-package consult-flyspell
  :bind (("M-s s" . consult-flyspell)))

;; * Company
;; ** main
(use-package company
  :hook
  (after-init . global-company-mode)
  (company-mode . yas-minor-mode)
  :bind ((:map company-mode-map
               ("M-<tab>" . company-manual-begin)
               ("C-M-i" . company-manual-begin)
               ("M-Y" . company-yasnippet)
               ("C-<tab>" . company-yasnippet)
               ("M-<iso-lefttab>" . company-dabbrev-ispell))
         (:map company-active-map
               ("<escape>" . company-abort)
               ("M->" . company-select-last)
               ("M-<" . company-select-first)
               ("<tab>" . company-complete-selection)))
  :init
  (setq company-idle-delay nil
        company-require-match 'never
        company-selection-wrap-around t
        company-minimum-prefix-length 1
        company-abort-on-unique-match nil
        company-abort-manual-when-too-short t
        company-icon-size '(auto-scale . 20)
        company-icon-margin 2
        company-tooltip-limit 14
        company-tooltip-align-annotations t
        company-tooltip-minimum-width 40
        company-dabbrev-minimum-length 4
        company-dabbrev-ignore-invisible t
        company-dabbrev-ignore-case 'keep-prefix
        company-dabbrev-downcase nil
        company-dabbrev-other-buffers 'all
        company-dabbrev-code-other-buffers t
        company-dabbrev-char-regexp "[[:word:]_-]+"
        company-dabbrev-ignore-buffers "\\.\\(?:pdf\\|gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)\\'"
        company-transformers '(company-sort-prefer-same-case-prefix)
        company-global-modes '(not message-mode help-mode
                                   vterm-mode eshell-mode)
        company-backends '(company-files
                           company-capf
                           (company-dabbrev :with company-ispell)
                           company-yasnippet))
  ;; use literal completion for company-mode
  (defun company-completion-styles (capf-fn &rest args)
    (let ((completion-styles '(basic partial-completion)))
      (apply capf-fn args)))
  (advice-add 'company-capf :around #'company-completion-styles)
  ;; start dabbrev with ispell
  (defun company-dabbrev-ispell ()
    (interactive)
    (let* ((company-backends '((company-dabbrev :with company-ispell))))
      (call-interactively 'company-manual-begin)))
  ;; prefix return nil when it's empty
  (defun company-backend--prefix-advice (orig-fun &rest args)
    (let ((prefix (apply orig-fun args)))
      (unless (string= (car prefix) "")
        prefix)))
  (advice-add 'company-dabbrev--prefix :around 'company-backend--prefix-advice)
  ;; auto-complete in text-mode
  (add-hook 'text-mode-hook (lambda ()
                              (setq-local company-idle-delay 0))))

(use-package company-prescient
  :hook
  (company-mode . company-prescient-mode)
  (company-prescient-mode . prescient-persist-mode))

;; ** frontend
(use-package company-posframe
  :if (zw/icon-displayable-p)
  :hook
  (company-mode . company-posframe-mode)
  :bind ((:map company-posframe-active-map
               ("s-d" . company-posframe-quickhelp-toggle)
               ("s-n" . company-posframe-quickhelp-scroll-up)
               ("s-p" . company-posframe-quickhelp-scroll-down)))
  :config
  (setq company-posframe-quickhelp-delay nil
        company-posframe-show-metadata nil
        company-posframe-show-indicator nil)
  ;; solve flickering
  (defun zw/company-posframe-show (orig-fun &rest args)
    (let ((x-wait-for-event-timeout 0)
          (pgtk-wait-for-event-timeout 0))
      (apply orig-fun args)))
  (advice-add 'company-posframe-show :around #'zw/company-posframe-show))

;; ** backend
(defun company-R-objects--prefix ()
  (unless (ess-inside-string-or-comment-p)
    (let ((start (ess-symbol-start)))
      (when start
        (buffer-substring-no-properties start (point))))))

(defun company-R-objects--candidates (arg)
  (let ((proc (ess-get-next-available-process)))
    (when proc
      (with-current-buffer (process-buffer proc)
        (all-completions arg (ess--get-cached-completions arg))))))

(defun company-capf-with-R-objects--check-prefix (prefix)
  (or (string-match-p "\\$" prefix)
      (string-match-p "\\@" prefix)))

(defun company-capf-with-R-objects (command &optional arg &rest rest)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-R-objects))
    (prefix (let ((prefix (company-R-objects--prefix)))
              (if (or (not prefix)
                      (string-match-p "\\:" prefix))
                  (company-grab-symbol)
                prefix)))
    (candidates (if (company-capf-with-R-objects--check-prefix arg)
                    (company-R-objects--candidates arg)
                  (company-capf--candidates arg (car rest))))
    (annotation (if (company-capf-with-R-objects--check-prefix arg)
                    "R-object"
                  (company-capf command arg rest)))
    (kind (if (company-capf-with-R-objects--check-prefix arg)
              'field
            (company-capf command arg rest)))
    (doc-buffer (company-capf command arg rest))))

(use-package company-reftex
  :commands (company-reftex-labels company-reftex-citations))

;; backends for prog-mode
(dolist (mode '(prog-mode-hook
                comint-mode-hook
                minibuffer-setup-hook
                inferior-python-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local company-minimum-prefix-length 2
                          company-backends
                          '(company-files company-capf)))))
;; backends for ess-r-mode
(add-hook 'ess-r-mode-hook
          (lambda ()
            (setq-local company-backends
                        ;; '(company-R-library company-R-objects company-files)
                        '(company-files company-capf-with-R-objects))))
(add-hook 'inferior-ess-r-mode-hook
          (lambda ()
            (setq-local company-backends
                        '(company-files company-R-library company-R-objects))))
;; backends for latex
(dolist (mode '(latex-mode-hook
                LaTeX-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local company-backends
                          '(company-files
                            company-reftex-labels company-reftex-citations
                            company-capf company-yasnippet)))))
;; backends for shell
(use-package company-shell
  :commands (company-shell)
  :hook (sh-mode . (lambda ()
                     (interactive)
                     (setq-local company-backends '(company-files company-shell)))))

;; * Provide
(provide 'zw-completion)
