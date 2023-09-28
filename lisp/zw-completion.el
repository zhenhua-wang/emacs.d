;; -*- lexical-binding: t -*-

;; * Orederless
;; orderless
(use-package orderless
  :config
  (orderless-define-completion-style orderless+initialism
    (orderless-matching-styles '(orderless-initialism
                                 orderless-literal
                                 orderless-regexp)))
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles basic partial-completion))
                                        (command (styles orderless+initialism))
                                        (variable (styles orderless+initialism))
                                        (symbol (styles orderless+initialism)))))

;; * Vertico
(use-package vertico
  :straight (vertico :files (:defaults "extensions/*")
                     :includes (vertico-directory))
  :hook
  (after-init . vertico-mode)
  (vertico-mode . vertico-multiform-mode)
  ;; Tidy shadowed file names
  (rfn-eshadow-update-overlay . vertico-directory-tidy)
  ;; More convenient directory navigation commands
  :bind ((:map vertico-map
               ("RET" . vertico-directory-enter)
               ("M-RET" . vertico-exit-input)
               ("DEL" . vertico-directory-delete-char)
               ("M-DEL" . vertico-directory-delete-word)))
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
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files))))

(use-package vertico-posframe
  :hook (vertico-mode . vertico-posframe-mode)
  :config
  (defun vertico-posframe-set-cursor (&rest args)
    (with-current-buffer vertico-posframe--buffer
      (setq-local cursor-type 'bar)
      (setq-local cursor-in-non-selected-windows 'bar)))
  (advice-add 'vertico-posframe--show :after 'vertico-posframe-set-cursor)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center
        vertico-posframe-width (frame-width)))

;; * Marginalia
(use-package marginalia
  :bind (("M-A" . marginalia-cycle)
         :map minibuffer-local-map
         ("M-A" . marginalia-cycle))
  :hook (vertico-mode . marginalia-mode)
  :config
  (setq marginalia-align 'center)
  ;; show mode on/off
  (defun marginalia-annotate-command (cand)
    "Annotate command CAND with its documentation string.
Similar to `marginalia-annotate-symbol', but does not show symbol class."
    (when-let* ((sym (intern-soft cand)))
      (concat
       (let ((mode (if (and sym (boundp sym))
                       sym
                     nil)))
         (when (and sym (boundp sym))
           (if (and (boundp mode) (symbol-value mode))
               (propertize " [On]" 'face 'marginalia-on)
             (propertize " [Off]" 'face 'marginalia-off))))
       (marginalia-annotate-binding cand)
       (marginalia--documentation (marginalia--function-doc sym))))))

;; * Consult
(use-package consult
  :demand
  :bind (;; C-c bindings (mode-specific-map)
         ("C-c h" . consult-history)
         ("C-c m" . consult-mode-command)
         ("C-c k" . consult-kmacro)
         ;; C-x bindings (ctl-x-map)
         ("C-x b" . consult-buffer)
         ("C-x C-b" . consult-buffer)
         ("C-x C-d" . consult-dir)
         ("C-x C-t" . consult-tramp)
         ;; Other custom bindings
         ("M-y" . consult-yank-pop)
         ("<help> a" . consult-apropos)
         ("s-f" . consult-line)
         ("s-F" . zw/consult-line-multi)
         ;; M-g bindings (goto-map)
         ("M-g g" . consult-goto-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ;; M-s bindings (search-map)
         ("M-s d" . consult-find)
         ("M-s g" . consult-grep)
         ("M-s y" . consult-yasnippet)
         ("M-s m" . consult-minor-mode-menu)
         ("M-s f" . consult-flymake)
         ("M-s s" . consult-flyspell)
         (:map isearch-mode-map
               ("M-s" . consult-isearch-history))
         (:map minibuffer-local-completion-map
               ("C-x C-d" . consult-dir)))
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
    (consult-line-multi t)))

;; other consult packages
(use-package consult-yasnippet
  :commands consult-yasnippet)
(use-package consult-dir
  :commands consult-dir)
(use-package consult-tramp
  :commands consult-tramp
  :straight (consult-tramp :host github :repo "Ladicle/consult-tramp")
  :init (setq consult-tramp-method "ssh"))
(use-package consult-flyspell
  :commands consult-flyspell)

;; * Provide
(provide 'zw-completion)
