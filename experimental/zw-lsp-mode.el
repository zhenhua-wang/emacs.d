;; -*- lexical-binding: t -*-

(setenv "LSP_USE_PLISTS" "true")
(setq-default read-process-output-max (* 1024 1024)
              lsp-use-plists t)
(use-package lsp-mode
  :commands (lsp-deferred)
  :hook
  (python-mode . zw/lsp-locally)
  (ess-r-mode . zw/lsp-locally)
  ((c++-mode c-mode) . zw/lsp-locally)
  :init
  (setq lsp-auto-guess-root t
        lsp-keep-workspace-alive nil
        lsp-keymap-prefix nil
        ;; modeline
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-headerline-breadcrumb-enable nil
        ;; doc
        lsp-eldoc-enable-hover nil
        lsp-signature-auto-activate nil
        lsp-signature-render-documentation nil
        ;; others
        lsp-enable-folding nil
        lsp-completion-provider :none
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-diagnostics-disabled-modes '(markdown-mode gfm-mode)
        lsp-enable-file-watchers nil
        lsp-enable-text-document-color nil)
  :config
  (defun zw/lsp-locally ()
    (unless (file-remote-p default-directory)
      (lsp-deferred))))

(use-package lsp-ui
  :commands (lsp-ui-imenu-buffer-mode)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-imenu-mode . zw/left-side-window-mode))
  :bind ((:map global-map
               ("s-i" . lsp-ui-imenu))
         (:map lsp-ui-imenu-mode-map
               ("s-i" . zw/kill-bufer-quit-window)
               ("<return>" . lsp-ui-imenu--visit)
               ("s-f" . isearch-forward))
         (:map lsp-ui-mode-map
               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
               ([remap xref-find-references] . lsp-ui-peek-find-references))
         (:map lsp-ui-doc-mode-map
               ("s-d" . lsp-ui-doc-toggle)))
  :init (setq lsp-ui-imenu-enable t
              lsp-ui-imenu-buffer-position 'left
              lsp-ui-imenu-auto-refresh 'after-save
              lsp-ui-imenu-auto-refresh-delay 0.5
              lsp-ui-sideline-enable nil
              lsp-ui-peek-always-show t
              lsp-ui-doc-position 'at-point
              lsp-ui-doc-max-width 120
              lsp-ui-doc-show-with-mouse nil)
  ;; use posframe to show peek
  (defun lsp-ui-peek--peek-display (src1 src2)
    (-let* ((win-width (frame-width))
            (lsp-ui-peek-list-width (/ (frame-width) 2))
            (string (-some--> (-zip-fill "" src1 src2)
                      (--map (lsp-ui-peek--adjust win-width it) it)
                      (-map-indexed 'lsp-ui-peek--make-line it)
                      (-concat it (lsp-ui-peek--make-footer)))))
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
  (advice-add #'lsp-ui-peek--peek-hide :override #'lsp-ui-peek--peek-destroy)
  ;; lsp-ui-imenu
  (defun zw/lsp-ui-imenu--modeline-name ()
    "Sidebar modeline name."
    (propertize (concat "imenu: " (buffer-name lsp-ui-imenu--origin)
                        zw/modeline-separator)
                'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                            'zw/modeline-default-inactive)))
  (setq lsp-ui-imenu--custom-mode-line-format
        (list "%e"
              '(:eval (zw/modeline-remote))
              '(:eval (zw/lsp-ui-imenu--modeline-name))
              '(:eval (zw/modeline-bar))))
  (defun zw/lsp-ui-imenu ()
    (interactive)
    (when (and (featurep 'lsp-mode) lsp-mode
               (not (buffer-base-buffer)))
      (ignore-errors
        (lsp-ui-imenu-buffer-mode 1)
        (setq lsp-ui-imenu--origin (current-buffer))
        (imenu--make-index-alist)
        (let ((imenu-buffer (get-buffer-create lsp-ui-imenu-buffer-name)))
          (lsp-ui-imenu--refresh-content)
          (let ((win (display-buffer-in-side-window
                      imenu-buffer '((side . left)
                                     (window-height . 0.4)))))
            (set-window-margins win 1)
            (set-window-start win 1)
            (lsp-ui-imenu--move-to-name-beginning)
            (set-window-dedicated-p win t)))))))

(provide 'zw-lsp-mode)
