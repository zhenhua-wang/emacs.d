;; -*- lexical-binding: t -*-

;; * Eglot
(use-package eglot
  :hook
  (prog-mode . (lambda ()
                 (unless (or (derived-mode-p 'emacs-lisp-mode 'lisp-mode
                                             'makefile-mode 'snippet-mode)
                             (file-remote-p default-directory))
                   (eglot-ensure))))
  ((latex-mode LaTeX-mode) . eglot-ensure)
  :bind (:map eglot-mode-map
              ([remap display-local-help] . nil)
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
  (setq jsonrpc-event-hook nil)
  (fset #'jsonrpc--log-event #'ignore)
  (setf (plist-get eglot-events-buffer-config :size) 0)
  ;; flymake-start after eglot publishDiagnostics
  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
      (with-current-buffer buffer
        (if (and (eq nil flymake-no-changes-timeout)
                 (not (buffer-modified-p)))
            (flymake-start t))))))

(use-package eglot-booster
  :when (executable-find "emacs-lsp-booster")
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :hook (after-init . eglot-booster-mode))

;; * Dape
(use-package dape
  :commands (dape dape-breakpoint-toggle zw/dape zw/dape-in-path zw/dape-in-path-menu)
  :bind (("<left-fringe> <mouse-1>" . dape-mouse-breakpoint-toggle)
         ("s-t" . dape-breakpoint-toggle)
         ("s-," . dape-breakpoint-expression)
         ("s-." . dape-breakpoint-log)
         ("s-/" . dape-watch-dwim)
         ("C-c C-d" . zw/dape))
  :hook (dape-info-parent-mode . zw/visual-line-disable)
  :config
  (setq dape-buffer-window-arrangement 'right
        dape-variable-auto-expand-alist '((hover . 1)))
  (add-hook 'dape-display-source-hook 'pulse-momentary-highlight-one-line)
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
      (plist-put current-config ':env `(:PYTHONPATH ,(dape-cwd)))
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
        (message "No debug program found for %s" major-mode)))))

;; * Folding
(use-package outline-minor-faces
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-mode))

;; * Comment
(use-package evil-nerd-commenter
  :bind (("s-;" . evilnc-comment-or-uncomment-lines)))

;; * Magit
(use-package magit
  :bind (("s-g" . magit-status)
         :map magit-mode-map
         ("C" . zw/magit-change-repo)
         ("q" . zw/magit-kill-buffers)
         :map magit-status-mode-map
         ("s-q" . zw/magit-kill-buffers))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1)
  ;; fix error #5551
  (remove-hook 'git-commit-setup-hook #'git-commit-setup-capf)
  (defun zw/magit-change-repo ()
    (interactive)
    (let ((dir (magit-read-repository)))
      (magit-kill-this-buffer)
      (magit-init dir)))
  (defun zw/magit-kill-buffers ()
    "Restore window configuration and kill all Magit buffers."
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers)))
  (defun zw/magit-remove-git-lock-file ()
    "Remove git's index lock file, if it exists."
    (interactive)
    (let ((base (magit-toplevel)))
      (when (y-or-n-p "Confirm deleting git index.lock?")
        (delete-file (expand-file-name ".git/index.lock" base)))))
  (with-eval-after-load "zw-tab-line"
    (add-to-list 'zw/tab-line-buffer-group-alist '(magit-buffer-file-name . File) :append)))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

;; * Eldoc box
(use-package eldoc-box
  :if (display-graphic-p)
  :bind (("<escape>" . zw/eldoc-box-keyboard-quit))
  :hook (eglot-managed-mode . eldoc-box-hover-mode)
  :config
  (defun zw/eldoc-box-keyboard-quit ()
    (interactive)
    (eldoc-box-quit-frame)
    (call-interactively 'keyboard-quit))
  (defun zw/eldoc-box--default-position-function (width _)
    (pcase-let ((`(,offset-l ,offset-r ,offset-t) eldoc-box-offset))
      (cons (- (frame-outer-width (selected-frame)) width offset-r)
            offset-t)))
  (setq eldoc-box-max-pixel-height 350
        eldoc-box-position-function 'zw/eldoc-box--default-position-function)
  (add-to-list 'eldoc-box-frame-parameters '(internal-border-width . 4))
  (add-hook 'zw/tab-line-before-kill-buffer-hook 'eldoc-box-reset-frame))

;; * Provide
(provide 'zw-ide)
