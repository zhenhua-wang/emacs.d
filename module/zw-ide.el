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
        eglot-send-changes-idle-time 0.5)
  ;; patch for polymode
  (with-eval-after-load "polymode"
    (defun zw/buffer-content (START END)
      (if (and (featurep 'polymode)
               polymode-mode)
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
      ;; HACK: apply Eglot patch and kill eglot.el buffer"
      (kill-buffer "eglot.el"))))

(use-package consult-eglot
  :commands (consult-eglot-symbols))

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

;; * REPL
(defvar zw/repl-env-path '(("~/.conda/envs/" . "bin/"))
  "Environment path should be formated as (env-dir . exec-dir).")

(defun zw/repl-path (&optional exec keep-tramp-prefix)
  (let* ((exec (or exec
                   (read-string "No exec is registered with current major mode.\nEnter manually: ")))
         (tramp-env-prefix (when (file-remote-p default-directory)
                             (let ((vec (tramp-dissect-file-name default-directory)))
                               (tramp-make-tramp-file-name
                                (tramp-file-name-method vec)
                                (tramp-file-name-user vec)
                                (tramp-file-name-domain vec)
                                (tramp-file-name-host vec)))))
         (exec-env-path (cl-mapcar (lambda (path)
                                     (cons (concat tramp-env-prefix (car path))
                                           (cdr path)))
                                   zw/repl-env-path)))
    (cons exec (cl-remove-if-not
                (lambda (full-path)
                  (file-exists-p (concat (when (not keep-tramp-prefix) tramp-env-prefix) full-path)))
                (apply #'append
                       (cl-mapcar
                        (lambda (dir)
                          (cl-mapcar (lambda (path)
                                       (let ((env-path (if (and tramp-env-prefix (not keep-tramp-prefix))
                                                           (string-replace tramp-env-prefix "" path)
                                                         path)))
                                         (expand-file-name exec (expand-file-name (cdr dir) env-path))))
                                     (ignore-errors (directory-files (car dir) t "^[^.]"))))
                        exec-env-path))))))

(defmacro zw/repl-run-in-path-macro (path-var repl-func &optional repl-args)
  (let ((path-var-symbol (eval path-var)))
    `(let* ((path (completing-read (format "Specify %s path: " ,path-var-symbol)
                                   (zw/repl-path ,path-var-symbol)))
            (,path-var-symbol path))
       (apply ,repl-func ,repl-args))))

(defun zw/repl-run-in-path ()
  (interactive)
  (pcase major-mode
    ('ess-r-mode
     (zw/repl-run-in-path-macro 'inferior-ess-r-program 'run-ess-r))
    ('python-mode
     (zw/repl-run-in-path-macro 'python-shell-interpreter 'run-python
                                (list nil (when (project-current) 'project) 'show)))
    (_ (message "No REPL is registered with current buffer"))))

;; * Provide
(provide 'zw-ide)
