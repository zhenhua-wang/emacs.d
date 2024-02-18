;; -*- lexical-binding: t -*-

;; * LSP
;; ** lsp-mode
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
  :commands (lsp-ui-imenu)
  :hook ((lsp-mode . lsp-ui-mode)
         (lsp-ui-imenu-mode . zw/lsp-ui-imenu-init))
  :bind ((("C-s-b" . zw/lsp-ui-imenu))
         (:map lsp-ui-imenu-mode-map
               ("C-s-b" . kill-buffer-and-window)
               ("<return>" . lsp-ui-imenu--visit)
               ("s-f" . isearch-forward))
         (:map lsp-ui-mode-map
               ([remap xref-find-definitions] . lsp-ui-peek-find-definitions)
               ([remap xref-find-references] . lsp-ui-peek-find-references))
         (:map lsp-ui-doc-mode-map
               ("s-d" . lsp-ui-doc-toggle)))
  :init (setq lsp-ui-imenu-enable t
              lsp-ui-imenu-buffer-position 'left
              lsp-ui-imenu-buffer-name "imenu"
              lsp-ui-imenu-window-width 0.3
              lsp-ui-imenu-window-fix-width t
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
  (setq lsp-ui-imenu--custom-mode-line-format
        (list "%e"
              '(:eval (zw/modeline-begin))
              '(:eval (zw/modeline-remote))
              '(:eval (zw/modeline-buffer-name 30 "..."))))
  (defun zw/lsp-ui-imenu-init ()
    (visual-line-mode -1)
    (setq-local buffer-face-mode-face
                (list :inherit 'tab-bar
                      :height (face-attribute 'default :height)
                      :box nil))
    (buffer-face-mode 1))
  (defun zw/lsp-ui-imenu ()
    (interactive)
    (ignore-errors
      (lsp-ui-imenu))))

;; ** eglot
(use-package eglot
  :straight (:type built-in)
  :bind (:map eglot-mode-map
              ([remap display-local-help] . nil)
              ("s-h" . display-local-help)
              ("s-d" . eldoc))
  :config
  (setq eglot-autoshutdown t
        eglot-send-changes-idle-time 0.5)
  ;; patch for polymode
  (with-eval-after-load "polymode"
    (defun zw/buffer-content (START END)
      (if (and (featurep 'polymode)
               polymode-mode)
          (pm--lsp-text)
        (buffer-substring-no-properties START END)))
    (let ((vc-follow-symlinks t))
      (psearch-patch eglot--TextDocumentItem
        (psearch-replace '`(buffer-substring-no-properties (point-min) (point-max))
                         '`(zw/buffer-content (point-min) (point-max)))))))

;; * Dape
(use-package dape
  :commands (zw/dape zw/dape-in-path dape dape-breakpoint-toggle)
  :bind (("<left-fringe> <mouse-1>" . dape-mouse-breakpoint-toggle))
  :config
  (setq dape-buffer-window-arrangement 'right)
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers t t)))
  ;; run dape in selected path
  (defun zw/dape-in-path (path)
    (interactive (list (completing-read "Specify command path: "
                                        (zw/lang-repl-path))))
    (let* ((current-config (cdr (cl-find-if
                                 (lambda (config)
                                   (and (not (eq (car config) 'current-config))
                                        (member major-mode (plist-get (cdr config) 'modes))))
                                 dape-configs)))
           (current-config (copy-tree current-config t)))
      ;; set 'current-config' command path
      (plist-put current-config 'command path)
      (plist-put current-config 'command-cwd (dape-command-cwd))
      (plist-put current-config ':cwd (dape-cwd))
      (plist-put current-config ':program (dape-buffer-default))
      ;; set tramp
      (when (file-remote-p default-directory)
        (setq current-config (dape-config-autoport (dape-config-tramp current-config))))
      ;; remove previous 'current-config'
      (setq dape-configs
            (cl-remove-if (lambda (config) (eq (car config) 'current-config)) dape-configs))
      ;; add new 'current-config'
      (add-to-list 'dape-configs `(current-config ,@current-config))
      (dape current-config)))
  (defun zw/dape ()
    (interactive)
    (let ((path (cdr (assq major-mode zw/lang-exec-alist))))
      (if path
          (zw/dape-in-path path)
        (message "No dape path found for %s" major-mode)))))

;; * Eldoc
(use-package eldoc-box
  :bind (([remap eldoc] . eldoc-box-help-at-point))
  :config
  (setq eldoc-box-clear-with-C-g t))

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
  (defface collapsed-face '((t (:inherit highlight))) "Collapsed Overlay")
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
  (prog-mode . zw-outline-mode)
  :bind
  ((:map outline-minor-mode-map
         ("<remap> <newline>" . zw/outline-newline)
         ("<remap> <delete-char>" . zw/outline-delete-char)
         ("<remap> <delete-backward-char>" . zw/outline-delete-backward-char)
         ("<remap> <backward-delete-char-untabify>" . zw/outline-backward-delete-char)))
  :config
  (defun zw/outline--level ()
    (length (match-string 2)))
  (defun zw/outline--unfontify (beg end)
    (let ((font-lock-extra-managed-props
           (append '(invisible) font-lock-extra-managed-props)))
      (font-lock-default-unfontify-region beg end)))
  (defun zw/outline-previous-invisible-p ()
    (unless (= (point) 1)
      (outline-invisible-p (- (point) 1))))
  (defun zw/outline-reveal-children ()
    (save-excursion
      (outline-back-to-heading)
      (outline-show-children) (outline-show-entry)))
  (defun zw/outline-reveal ()
    (cond
     ;; visible
     ((not (outline-invisible-p)) nil)
     ;; invisible, has sub
     ((outline-has-subheading-p)
      (zw/outline-reveal-children) (zw/outline-reveal))
     ;; invisible, no sub
     (t (zw/outline-reveal-children))))
  (defun zw/outline-newline (&optional ARG INTERACTIVE)
    (interactive "*P\np")
    (newline ARG INTERACTIVE)
    (zw/outline-reveal))
  (defun zw/outline-delete-char (N)
    (interactive "p")
    (delete-char N)
    (zw/outline-reveal))
  (defun zw/outline-delete-backward-char (n &optional killflag)
    (declare (interactive-only delete-char))
    (interactive "p\nP")
    (delete-backward-char n killflag)
    (save-excursion
      (backward-char)
      (zw/outline-reveal)))
  (defun zw/outline-backward-delete-char (ARG &optional KILLP)
    (interactive "p\nP")
    (backward-delete-char-untabify ARG KILLP)
    (save-excursion
      (backward-char)
      (zw/outline-reveal)))
  (defvar-local zw/outline--font-lock-keywords nil)
  (define-minor-mode zw-outline-mode
    "Toggle zw-outline mode."
    :global nil
    (let* ((comment-start-symbol (or (string-trim comment-start) "#"))
           (outline-header (rx-to-string
                            `(: (group (0+ space)
                                       (+ ,comment-start-symbol)
                                       (+ space) (group (+ "*")))
                                space))))
      (cond
       (zw-outline-mode
        (add-to-invisibility-spec 'zw-outline-star)
        (setq-local comment-start-symbol (or (string-trim comment-start) "#")
                    outline-regexp (rx-to-string
                                    `(: (group (0+ space)
                                               (+ ,comment-start-symbol)
                                               (+ space) (group (+ "*")))
                                        space))
                    zw/outline--font-lock-keywords `((,outline-regexp
                                                      1 '(face nil invisible zw-outline-star)))
                    outline-level 'zw/outline--level
                    outline-minor-mode-use-buttons t
                    font-lock-unfontify-region-function #'zw/outline--unfontify
                    outline-isearch-open-invisible-function (lambda (o) (zw/outline-reveal)))
        (font-lock-add-keywords nil zw/outline--font-lock-keywords)
        (outline-minor-mode 1)
        (outline-hide-sublevels 1)
        (add-hook 'post-self-insert-hook 'zw/outline-reveal nil t)
        (add-hook 'save-place-after-find-file-hook 'zw/outline-reveal nil t))
       (t
        ;; unfontify
        (remove-from-invisibility-spec 'zw-outline-star)
        (dolist (o (overlays-in (window-start) (window-end)))
          (when (overlay-get o 'outline-button)
            (delete-overlay o)))
        ;; reset config
        (setq-local outline-regexp (default-value 'outline-regexp)
                    outline-level (default-value 'outline-level)
                    outline-minor-mode-use-buttons nil
                    font-lock-unfontify-region-function #'font-lock-default-unfontify-region
                    outline-isearch-open-invisible-function #'outline-isearch-open-invisible)
        (font-lock-remove-keywords nil zw/outline--font-lock-keywords)
        (outline-minor-mode 0)
        (remove-hook 'post-self-insert-hook 'zw/outline-reveal t)
        (remove-hook 'save-place-after-find-file-hook 'zw/outline-reveal t))))))

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
  :bind (("s-G" . magit-status)
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
