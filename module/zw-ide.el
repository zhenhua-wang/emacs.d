;; -*- lexical-binding: t -*-

;; * Eglot
(use-package eglot
  :hook
  (prog-mode . (lambda ()
                 (unless (or (and (featurep 'polymode) polymode-mode)
                             (file-remote-p default-directory))
                   (eglot-ensure))))
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
  ;; flymake-start after eglot publishDiagnostics
  (cl-defmethod eglot-handle-notification :after
    (_server (_method (eql textDocument/publishDiagnostics)) &key uri
             &allow-other-keys)
    (when-let ((buffer (find-buffer-visiting (eglot-uri-to-path uri))))
      (with-current-buffer buffer
        (if (and (eq nil flymake-no-changes-timeout)
                 (not (buffer-modified-p)))
            (flymake-start t))))))

(use-package consult-eglot
  :after eglot
  :bind (:map eglot-mode-map
              ("s-i" . consult-eglot-symbols)))

;; * Dape
(use-package dape
  :commands (dape dape-breakpoint-toggle zw/dape zw/dape-in-path zw/dape-in-path-menu)
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
        (message "No debug program found for %s" major-mode)))))

;; * Treesit
(use-package treesit-auto
  :if (and (fboundp 'treesit-available-p)
           (treesit-available-p))
  :hook ((after-init . global-treesit-auto-mode)
         (change-major-mode . zw/treesit-auto-remap))
  :init
  (setq treesit-auto-install 'prompt)
  :config
  (defun zw/treesit-auto--get-buffer-recipe ()
    (seq-find
     (lambda (r)
       (ignore-errors
         (string-match (treesit-auto-recipe-ext r) (buffer-name))))
     (treesit-auto--selected-recipes)))
  (defun zw/treesit-auto-remap ()
    (when-let* ((recipe (zw/treesit-auto--get-buffer-recipe))
                (lang (treesit-auto-recipe-lang recipe))
                (ts-mode (treesit-auto-recipe-ts-mode recipe))
                (mode (treesit-auto-recipe-remap recipe))
                (mode-hook (intern (format "%s-hook" mode)))
                (mode-map (intern (format "%s-map" mode)))
                (ts-mode-hook (intern (format "%s-hook" ts-mode)))
                (ts-mode-map (intern (format "%s-map" ts-mode))))
      (with-eval-after-load lang
        (when (and (boundp mode-hook))
          (eval `(setf ,ts-mode-hook ,mode-hook)))
        (when (and (boundp mode-map))
          (eval `(setf ,ts-mode-map ,mode-map)))))))

;; * Eldoc
(use-package eldoc-box
  :bind (([remap eldoc] . eldoc-box-help-at-point))
  :config
  (setq eldoc-box-clear-with-C-g t))

;; * Folding
(use-package outline-minor-faces
  :after outline
  :hook (outline-minor-mode . outline-minor-faces-mode))

;; * Comment
(use-package evil-nerd-commenter
  :bind (("s-;" . evilnc-comment-or-uncomment-lines)))

;; * Magit
(use-package magit
  :bind (("s-G" . magit-status)
         :map magit-mode-map
         ("C" . zw/magit-change-repo)
         ("s-q" . magit-mode-bury-buffer))
  :commands (magit-status magit-get-current-branch)
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1
        magit-bury-buffer-function #'magit-restore-window-configuration)
  (defun zw/magit-change-repo ()
    (interactive)
    (let ((dir (magit-read-repository)))
      (magit-kill-this-buffer)
      (magit-init dir))))

(use-package magit-todos
  :hook (magit-mode . magit-todos-mode))

;; * Tabspaces
(use-package tabspaces
  :hook (after-init . tabspaces-mode)
  :custom
  (tabspaces-use-filtered-buffers-as-default t)
  (tabspaces-default-tab "Main")
  (tabspaces-remove-to-default t)
  (tabspaces-include-buffers '("*scratch*" "*Messages*"))
  ;; sessions
  (tabspaces-session t)
  (tabspaces-session-auto-restore t)
  :init
  ;; filter Buffers for Consult-Buffer
  (with-eval-after-load 'consult
    ;; hide full buffer list (still available with "b" prefix)
    (consult-customize consult--source-buffer :hidden t :default nil)
    ;; set consult-workspace buffer list
    (defvar consult--source-workspace
      (list :name     "Workspace Buffer"
            :narrow   ?w
            :history  'buffer-name-history
            :category 'buffer
            :state    #'consult--buffer-state
            :default  t
            :items    (lambda () (consult--buffer-query
                                  :predicate #'tabspaces--local-buffer-p
                                  :sort 'visibility
                                  :as #'buffer-name)))
      "Set workspace buffer list for consult-buffer.")
    (add-to-list 'consult-buffer-sources 'consult--source-workspace))
  ;; filter tab-lines
  (with-eval-after-load "zw-tab-line"
    (defun zw/tabspace-filter-tab-line (old-func)
      (cl-remove-if-not (lambda (buffer)
                          (memq buffer (tabspaces--buffer-list)))
                        (funcall old-func)))
    (advice-add 'zw/tab-line-buffer-group-buffers :around
                #'zw/tabspace-filter-tab-line))
  ;; open dashboard in default tab
  (with-eval-after-load "dashboard"
    (defun zw/tabspace-dashboard ()
      (tab-bar-select-tab-by-name tabspaces-default-tab))
    (advice-add 'dashboard-open :before #'zw/tabspace-dashboard)
    (advice-add 'dashboard-initialize :before #'zw/tabspace-dashboard)))

;; * Provide
(provide 'zw-ide)
