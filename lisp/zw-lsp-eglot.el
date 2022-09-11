(setq read-process-output-max (* 1024 1024)) ;; 1mb

(use-package lsp-mode
  :commands (lsp lsp-org)
  :hook
  ;; (python-mode . lsp-deferred)
  ;; (c++-mode . lsp-deferred)
  ;; (ess-r-mode . lsp-deferred)
  ;; ((latex-mode LaTeX-mode) . lsp-deferred)
  ((ess-r-mode latex-mode LaTeX-mode) . disable-lsp-imenu)
  (lsp-completion-mode . my/lsp-mode-setup-completion)
  :init
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-headerline-breadcrumb-enable nil
        lsp-signature-render-documentation nil

        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-text-document-color nil

        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil

        lsp-idle-delay 0.1
        lsp-completion-provider :none
        lsp-tex-server 'digestif)
  (defun my/lsp-mode-setup-completion ()
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  :config
  (defun lsp-update-server ()
    "Update LSP server."
    (interactive)
    ;; Equals to `C-u M-x lsp-install-server'
    (lsp-install-server t))
  ;; BUG: disable imenu for ess-r-mode https://github.com/REditorSupport/languageserver/issues/558
  (defun disable-lsp-imenu ()
    (if (featurep 'lsp-mode)
        (setq-local lsp-enable-imenu nil))))

;; microsoft pyright
(use-package lsp-pyright
  :after lsp-mode
  :config (setq lsp-pyright-auto-import-completions nil))

(use-package lsp-ui
  :custom-face
  (lsp-ui-sideline-code-action ((t (:inherit warning))))
  :bind ((:map lsp-ui-mode-map
               ("s-<return>" . lsp-ui-sideline-apply-code-actions)
               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
               ([remap xref-find-references] . lsp-ui-peek-find-references))
         (:map lsp-ui-doc-mode-map
               ("s-d" . lsp-ui-doc-show)))
  :hook (lsp-mode . lsp-ui-mode)
  :init
  (setq lsp-ui-peek-always-show t
        lsp-ui-sideline-show-diagnostics nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-code-actions nil
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse nil
        lsp-ui-doc-position 'top
        lsp-ui-doc-delay 0.1)
  ;; Set correct color to borders
  (defun my-lsp-ui-doc-set-border ()
    "Set the border color of lsp doc."
    (setq lsp-ui-doc-border
          (if (facep 'posframe-border)
              (face-background 'posframe-border nil t)
            (face-foreground 'shadow nil t))))
  (my-lsp-ui-doc-set-border)
  (add-hook 'after-load-theme-hook #'my-lsp-ui-doc-set-border t)
  :config
  ;; use posframe to show peek
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      (--map (lsp-ui-peek--adjust win-width it) it)
                      (-map-indexed 'lsp-ui-peek--make-line it)
                      (-concat it (lsp-ui-peek--make-footer))))
            )
      (setq lsp-ui-peek--buffer (get-buffer-create " *lsp-peek--buffer*"))
      (posframe-show lsp-ui-peek--buffer
                     :string (mapconcat 'identity string "")
                     :min-width (frame-width)
                     :poshandler #'posframe-poshandler-frame-center
                     :override-parameters '((tab-bar-mode . 0)
                                            (tab-bar-format . nil)
                                            (tab-line-format . nil)
                                            (tab-bar-lines . 0)
                                            (tab-bar-lines-keep-state . 0)))))
  (defun lsp-ui-peek--peek-destroy ()
    (when (bufferp lsp-ui-peek--buffer)
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
          lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))
  (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy))

;; note enable eglot would override "M-." key to xref
(use-package eglot
  :commands (eglot)
  :config
  (setq eglot-stay-out-of '(flymake imenu)
        eglot-autoshutdown t)
  (setq-default eglot-workspace-configuration
                '((:pyright . ((useLibraryCodeForTypes . t))))))

;; use lsp-mode in local and eglot in remote
(dolist (mode '(python-mode-hook
                c++-mode-hook c-mode-hook
                latex-mode-hook LaTeX-mode-hook))
  (add-hook mode (lambda ()
                   (if (file-remote-p default-directory)
                       (eglot-ensure)
                     (lsp)))))
;; use lsp-mode only in local
(dolist (mode '(ess-r-mode-hook))
  (add-hook mode (lambda ()
                   (when (not (file-remote-p default-directory))
                     (lsp)))))

;; enable lsp in org babel
(with-eval-after-load 'org
  ;; enable lsp-org inside src block
  (cl-defmacro lsp-org-babel-enable (lang)
    "Support LANG in org source code block."
    (setq centaur-lsp 'lsp-mode)
    (cl-check-type lang stringp)
    (let* ((edit-pre (intern (format "org-babel-edit-prep:%s" lang)))
           (intern-pre (intern (format "lsp--%s" (symbol-name edit-pre)))))
      `(progn
         (defun ,intern-pre (info)
           (let ((file-name (->> info caddr (alist-get :file))))
             (unless file-name
               (setq file-name (make-temp-file "babel-lsp-")))
             (setq buffer-file-name file-name)
             (lsp-deferred)))
         (put ',intern-pre 'function-documentation
              (format "Enable lsp-mode in the buffer of org source block (%s)."
                      (upcase ,lang)))
         (if (fboundp ',edit-pre)
             (advice-add ',edit-pre :after ',intern-pre)
           (progn
             (defun ,edit-pre (info)
               (,intern-pre info))
             (put ',edit-pre 'function-documentation
                  (format "Prepare local buffer environment for org source block (%s)."
                          (upcase ,lang))))))))
  (defvar org-babel-lang-list
    '("ess-r" "R" "python" "latex"))
  (dolist (lang org-babel-lang-list)
    (eval `(lsp-org-babel-enable ,lang))))


(provide 'zw-lsp-eglot)
