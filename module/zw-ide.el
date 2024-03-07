;; -*- lexical-binding: t -*-

;; * Eglot
(use-package eglot
  :hook
  (python-mode . eglot-ensure)
  (ess-r-mode . eglot-ensure)
  ((c++-mode c-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ([remap display-local-help] . nil)
              ("s-i" . consult-eglot-symbols)
              ("s-h" . display-local-help)
              ("s-d" . eldoc))
  :config
  (setq read-process-output-max (* 1024 1024)
        eglot-autoshutdown t
        eglot-sync-connect nil
        eglot-connect-timeout nil
        eglot-send-changes-idle-time 0.5
        eglot-stay-out-of '(company)
        eglot-ignored-server-capabilities '(:documentOnTypeFormattingProvider))
  ;; speedup eglot
  (fset #'jsonrpc--log-event #'ignore)
  (setf (plist-get eglot-events-buffer-config :size) 0)
  ;; HACK: flymake-start after eglot publishDiagnostics
  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
      (with-current-buffer buffer
        (if (and (eq nil flymake-no-changes-timeout)
                 (not (buffer-modified-p)))
            (flymake-start t)))))
  ;; patch for polymode
  (with-eval-after-load "polymode"
    (defun zw/buffer-content (START END)
      (if (and polymode-mode pm/polymode)
          (pm--lsp-text)
        (buffer-substring-no-properties START END)))
    (defmacro zw/eglot-patch-macro (patch-func)
      `(psearch-patch ,patch-func
         (psearch-replace '`(buffer-substring-no-properties (point-min) (point-max))
                          '`(zw/buffer-content (point-min) (point-max)))))
    (let ((vc-follow-symlinks t))
      (dolist (func '(eglot--TextDocumentItem
                      eglot--signal-textDocument/didSave
                      eglot--signal-textDocument/didChange))
        (eval `(zw/eglot-patch-macro ,func)))
      ;; HACK: kill eglot.el buffer after applying Eglot patch"
      (kill-buffer "eglot.el"))))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("s-i" . consult-eglot-symbols)))

;; * Dape
(use-package dape
  :commands (zw/dape zw/dape-in-path zw/dape-in-path-menu dape dape-breakpoint-toggle)
  :bind (("<left-fringe> <mouse-1>" . dape-mouse-breakpoint-toggle))
  :config
  (setq dape-buffer-window-arrangement 'right)
  ;; Save buffers on startup, useful for interpreted languages
  (add-hook 'dape-on-start-hooks
            (defun dape--save-on-start ()
              (save-some-buffers t t)))
  ;; run dape in selected path
  (defun zw/dape-major-mode-config ()
    (cdr (cl-find-if
          (lambda (config)
            (and (not (eq (car config) 'current-config))
                 (member major-mode (plist-get (cdr config) 'modes))))
          dape-configs)))
  (defun zw/dape-in-path (path)
    (interactive (list (completing-read "Specify command path: "
                                        (zw/repl-path
                                         (plist-get (zw/dape-major-mode-config)
                                                    'command)))))
    (let ((current-config (copy-tree (zw/dape-major-mode-config) t)))
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
  (defun zw/dape-in-path-menu ()
    (interactive)
    (zw/define-menu
     "Debug path"
     (append (mapcar (lambda (x)
                       (vector x `(lambda () (interactive) (zw/dape-in-path ,x) t)))
                     (zw/repl-path
                      (plist-get (zw/dape-major-mode-config) 'command))))))
  (defun zw/dape ()
    (interactive)
    (let* ((current-config (zw/dape-major-mode-config))
           (path (plist-get current-config 'command)))
      (if path
          (zw/dape-in-path path)
        (message "No dape path found for %s" major-mode)))))

;; * Treesit
(use-package treesit-auto
  :hook ((after-init . global-treesit-auto-mode)
         (prog-mode . zw/treesit-auto-remap))
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (defun zw/treesit-auto-remap ()
    (when-let* ((recipe (treesit-auto--get-buffer-recipe))
                (lang (treesit-auto-recipe-lang recipe))
                (ts-mode (treesit-auto-recipe-ts-mode recipe))
                (mode (treesit-auto-recipe-remap recipe))
                (mode-hook (intern (format "%s-hook" mode)))
                (mode-map (intern (format "%s-map" mode)))
                (ts-mode-hook (intern (format "%s-hook" ts-mode)))
                (ts-mode-map (intern (format "%s-map" ts-mode))))
      (with-eval-after-load lang
        (eval `(setf ,ts-mode-hook ,mode-hook))
        ;; hack: remap mode-map to ts-mode-map
        (eval `(setf ,ts-mode-map ,mode-map))))))

;; * Eldoc
(use-package eldoc-box
  :bind (([remap eldoc] . eldoc-box-help-at-point))
  :config
  (setq eldoc-box-clear-with-C-g t))

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
  :config
  (defun zw/outline--level ()
    (length (match-string 2)))
  (defun zw/outline--unfontify (beg end)
    (let ((font-lock-extra-managed-props
           (cons 'invisible font-lock-extra-managed-props)))
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
    :keymap
    `((,(kbd "<remap> <newline>") . zw/outline-newline)
      (,(kbd "<remap> <delete-char>") . zw/outline-delete-char)
      (,(kbd "<remap> <delete-backward-char>") . zw/outline-delete-backward-char)
      (,(kbd "<remap> <backward-delete-char-untabify>") . zw/outline-backward-delete-char))
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
      (remove-hook 'save-place-after-find-file-hook 'zw/outline-reveal t)))))

(use-package outline-minor-faces
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-mode))

;; * Fast comment
(use-package evil-nerd-commenter
  :bind (("s-;" . evilnc-comment-or-uncomment-lines)))

;; * Magit
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
