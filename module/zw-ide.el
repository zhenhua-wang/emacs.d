;; -*- lexical-binding: t -*-

;; * LSP
(setq-default read-process-output-max (* 1024 1024))
(use-package lsp-mode
  :commands (lsp-deferred)
  :hook
  (python-mode . lsp-deferred)
  (ess-r-mode . lsp-deferred)
  ((c++-mode c-mode) . lsp-deferred)
  :init
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        ;; modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-headerline-breadcrumb-enable nil
        ;; doc
        lsp-eldoc-enable-hover nil
        lsp-signature-render-documentation nil
        ;; others
        lsp-enable-folding nil
        lsp-completion-provider :none
        lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind ((:map lsp-ui-mode-map
               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
               ([remap xref-find-references] . lsp-ui-peek-find-references))
         (:map lsp-ui-doc-mode-map
               ("s-d" . lsp-ui-doc-toggle)))
  :init (setq lsp-ui-imenu-enable nil
              lsp-ui-sideline-enable nil
              lsp-ui-peek-always-show t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-max-width 120
              lsp-ui-doc-show-with-mouse nil)
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
                     :poshandler #'posframe-poshandler-frame-center)))
  (defun lsp-ui-peek--peek-destroy ()
    (when (bufferp lsp-ui-peek--buffer)
      (posframe-delete lsp-ui-peek--buffer))
    (setq lsp-ui-peek--buffer nil
          lsp-ui-peek--last-xref nil)
    (set-window-start (get-buffer-window) lsp-ui-peek--win-start))
  (advice-add #'lsp-ui-peek--peek-new :override #'lsp-ui-peek--peek-display)
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy))

;; * Code reference
(use-package xref
  :straight (:type built-in)
  :init
  (when (executable-find "rg")
    (setq xref-search-program 'ripgrep))
  :config
  (setq xref-prompt-for-identifier '(not xref-find-definitions
                                         xref-find-definitions-other-window
                                         xref-find-definitions-other-frame
                                         xref-find-references)))

;; * Code folding
;; ** hideshow
(use-package hideshow
  :hook ((prog-mode org-mode markdown-mode) . hs-minor-mode)
  :bind ((:map hs-minor-mode-map ("<backtab>" . zw/toggle-fold)))
  :config
  (defun zw/toggle-fold ()
    "Toggle code folding"
    (interactive)
    (save-excursion
      (end-of-line)
      (hs-toggle-hiding)))
  (defface collapsed-face '((t (:background "#e0cf9f" :foreground "#5f5f5f"))) "Collapsed Overlay")
  (defvar collapsed-face 'collapsed-face)
  (define-fringe-bitmap 'hs-marker [0 24 24 126 126 24 24 0])
  (defun display-code-line-counts (ov)
    (when (eq 'code (overlay-get ov 'hs))
      (let* ((marker-string "*fringe-dummy*")
	     (marker-length (length marker-string))
	     (display-string
	      (format " (%d lines)... "
		      (count-lines (overlay-start ov) (overlay-end ov)))))
        (overlay-put ov 'help-echo "<backtab> to toggle")
        (put-text-property 0 marker-length 'display
			   (list 'left-fringe 'hs-marker 'fringe-face)
			   marker-string)
        (overlay-put ov 'before-string marker-string)
        (put-text-property 1 (1- (length display-string))
			   'face 'collapsed-face display-string)
        (overlay-put ov 'display display-string))))
  (setq hs-set-up-overlay 'display-code-line-counts))

;; ** outline
(use-package outline
  :hook
  (prog-mode . zw/outline-init)
  :bind
  ((:map outline-minor-mode-map
         ("<DEL>" . zw/outline-backward-delete-char)
         ("<remap> <self-insert-command>" . zw/outline-self-insert-command)
         ("<remap> <newline>" . zw/outline-newline)
         ("<remap> <delete-char>" . zw/outline-delete-char)))
  :config
  (setq outline-minor-mode-use-buttons t)
  (defun zw/outline--level ()
    (length (match-string 2)))
  (defun zw/outline--unfontify (beg end &optional _loud)
    (let ((font-lock-extra-managed-props
           (append '(display) font-lock-extra-managed-props)))
      (font-lock-default-unfontify-region beg end)))
  (defun zw/outline-previous-invisible-p ()
    (save-excursion
      (backward-char)
      (outline-invisible-p)))
  (defun zw/outline-show-entry ()
    (cond
     ((and (outline-has-subheading-p) (outline-invisible-p) (outline-on-heading-p)) (outline-show-branches))
     ((outline-has-subheading-p) (outline-show-branches) (zw/outline-show-entry))
     (t (outline-show-entry))))
  (defun zw/outline-self-insert-command (N &optional C)
    (interactive "p")
    (when (outline-invisible-p)
      (zw/outline-show-entry))
    (self-insert-command N C))
  (defun zw/outline-newline (&optional ARG INTERACTIVE)
    (interactive "p")
    (when (outline-invisible-p)
      (zw/outline-show-entry))
    (newline ARG INTERACTIVE))
  (defun zw/outline-delete-char (N)
    (interactive "p")
    (when (outline-invisible-p)
      (zw/outline-show-entry))
    (delete-char N))
  (defun zw/outline-backward-delete-char (N)
    (interactive "p")
    (when (zw/outline-previous-invisible-p)
      (zw/outline-show-entry))
    (backward-delete-char N))
  (defun zw/outline-init ()
    (let* ((comment-start-symbol (or (string-trim comment-start) "#"))
           (outline-header (rx-to-string
                            `(: (group (0+ space)
                                       (+ ,comment-start-symbol)
                                       (+ space) (group (+ "*")))
                                space))))
      (font-lock-add-keywords nil `((,outline-header 1 '(face nil display ""))))
      (setq-local outline-regexp outline-header
                  outline-level 'zw/outline--level
                  font-lock-unfontify-region-function #'zw/outline--unfontify))
    (outline-minor-mode 1)
    (outline-hide-sublevels 1)
    (add-hook 'save-place-after-find-file-hook
              (lambda () (when (outline-invisible-p) (zw/outline-show-entry)) nil t))))

(use-package outline-minor-faces
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-mode))

;; * Fast comment
(use-package evil-nerd-commenter
  :bind (("s-;" . evilnc-comment-or-uncomment-lines)))

;; * Flymake
(use-package flymake
  :straight (:type built-in)
  :hook (prog-mode . flymake-mode)
  :config
  (setq flymake-no-changes-timeout nil
        flymake-fringe-indicator-position nil)
  ;; disable flymake log about proc-legacy-flymake
  (remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
  ;; show flymake when cursor hovers
  (setq help-at-pt-timer-delay 0.9
        help-at-pt-display-when-idle 'never))

;; * VC
(use-package magit
  :bind (("s-M" . magit-status)
         :map magit-mode-map
         ("C" . zw/magit-change-repo))
  :commands (magit-status magit-get-current-branch)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  (defun zw/magit-change-repo ()
    (interactive)
    (let ((dir (magit-read-repository)))
      (magit-kill-this-buffer)
      (magit-init dir))))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

;; * Provide
(provide 'zw-ide)
