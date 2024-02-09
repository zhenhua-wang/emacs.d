;; -*- lexical-binding: t -*-

;; * Orederless
;; orderless
(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        orderless-component-separator #'orderless-escapable-split-on-space
        completion-category-overrides '((file (styles partial-completion)))))

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
    (nconc (cl-remove-if-not (lambda (x) (string-suffix-p "/" x)) files)
           (cl-remove-if (lambda (x) (string-suffix-p "/" x)) files))))

(use-package vertico-posframe
  :if (zw/icon-displayable-p)
  :hook (vertico-mode . vertico-posframe-mode)
  :bind (:map vertico-multiform-map
              ("M-p" . nil))
  :config
  (defun vertico-posframe-set-cursor (&rest args)
    (with-current-buffer vertico-posframe--buffer
      (setq-local cursor-type 'bar)
      (setq-local cursor-in-non-selected-windows 'bar)))
  (advice-add 'vertico-posframe--show :after 'vertico-posframe-set-cursor)
  (setq vertico-posframe-poshandler 'posframe-poshandler-frame-bottom-center
        vertico-posframe-width (/ (display-pixel-width)
                                  (frame-char-width))))

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
         ("C-x C-t" . zw/consult-dir-tramp-ssh)
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
  :commands consult-dir zw/consult-dir-tramp-ssh
  :config
  (defun zw/consult-dir-tramp-ssh ()
    (interactive)
    (let ((consult-dir-sources '(consult-dir--source-tramp-ssh)))
      (consult-dir))))
(use-package consult-flyspell
  :commands consult-flyspell)

;; * Company
;; ** main
(use-package company
  :hook
  (after-init . global-company-mode)
  (company-mode . yas-minor-mode)
  (ess-r-mode . company-mode)
  :bind ((:map company-mode-map
               ("M-<tab>" . company-other-backend)
               ("M-<iso-lefttab>" . company-dabbrev-ispell))
         (:map company-active-map
               ("<escape>" . company-abort)
               ("M->" . company-select-last)
               ("M-<" . company-select-first)
               ("<tab>" . company-complete-selection)
               ("C-<tab>" . company-yasnippet)))
  :init (setq company-idle-delay nil
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
              company-dabbrev-ignore-buffers "\\.\\(?:pdf\\|jpe?g\\|png\\)\\'"
              company-transformers '(company-sort-prefer-same-case-prefix)
              company-global-modes '(not message-mode help-mode
                                         vterm-mode eshell-mode)
              company-backends '(company-files
                                 company-capf
                                 company-yasnippet
                                 (company-dabbrev :with company-ispell)))
  ;; remove completions that start with numbers
  (push (apply-partially #'cl-remove-if
                         (lambda (c) (string-match-p "\\`[0-9]+" c)))
        company-transformers)
  ;; start dabbrev with ispell
  (defun company-dabbrev-ispell ()
    (interactive)
    (let* ((prefix (company-grab-symbol))
           (company-backends '((company-dabbrev :with company-ispell))))
      (unless (string= prefix "")
        (call-interactively 'company-manual-begin)))))

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
        company-posframe-show-indicator t)
  ;; set show parameters
  (defun zw/company-posframe-refposhandler (&optional frame)
    (cond
     ((bound-and-true-p exwm--connection)
      (or (with-slots ((x* x) (y* y))
              (exwm-workspace--workarea frame)
            (cons x* y*))
          (posframe-refposhandler-xwininfo frame)
          (cons 0 0)))
     (t nil)))
  (defun zw/company-posframe-quickhelp-refposhandler (&optional frame)
    (cond
     ((bound-and-true-p exwm--connection) (cons 0 0))
     (t . nil)))
  (setq company-posframe-quickhelp-show-params
        (list :refposhandler 'zw/company-posframe-quickhelp-refposhandler
              :poshandler 'company-posframe-quickhelp-right-poshandler
              :timeout 60
              :no-properties nil))
  (defun zw/company-posframe-show-params ()
    (setq company-posframe-show-params
          (list :refposhandler 'zw/company-posframe-refposhandler
                :override-parameters
                `((tab-bar-mode . 0)
                  (tab-bar-format . nil)
                  (tab-line-format . nil)
                  (tab-bar-lines . 0)
                  (tab-bar-lines-keep-state . 0)
                  (background-color . ,(zw/get-face-bg-recur 'company-tooltip))))))
  (advice-add #'company-posframe-show :before #'zw/company-posframe-show-params))

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
  (or (cl-search "$" prefix)
      (cl-search "@" prefix)))

(defun company-capf-with-R-objects (command &optional arg &rest ignored)
  (interactive (list 'interactive))
  (cl-case command
    (interactive (company-begin-backend 'company-R-objects))
    (prefix (company-R-objects--prefix))
    (candidates (if (company-capf-with-R-objects--check-prefix arg)
                    (company-R-objects--candidates arg)
                  (company-capf command arg)))
    (annotation (if (company-capf-with-R-objects--check-prefix arg)
                    "R-object"
                  (company-capf command arg)))
    (kind (if (company-capf-with-R-objects--check-prefix arg)
              'field
            (company-capf command arg)))
    (doc-buffer (company-capf command arg))))

(use-package company-reftex
  :commands (company-reftex-labels company-reftex-citations))

;; backends for prog-mode
(dolist (mode '(prog-mode-hook
                minibuffer-setup-hook
                inferior-python-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local company-backends
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
