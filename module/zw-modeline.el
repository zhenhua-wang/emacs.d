;; -*- lexical-binding: t -*-

(defgroup zw/modeline nil
  "zw/modeline"
  :group 'convenience)

(defgroup zw/modeline-active nil
  "zw/modeline-active"
  :group 'zw/modeline)

(defgroup zw/modeline-inactive nil
  "zw/modeline-inactive"
  :group 'zw/modeline)

(defface zw/modeline-default-active
  `((t (:foreground ,(face-foreground 'mode-line))))
  "Default face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-default-inactive
  `((t (:foreground ,(face-foreground 'font-lock-comment-face))))
  "Default face for inactive modeline"
  :group 'zw/modeline-inactive)

(defface zw/modeline-highlight-foreground-active
  `((t (:foreground ,(face-background 'mode-line-highlight))))
  "Highlight foreground face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-highlight-background-active
  '((t (:inherit mode-line-highlight)))
  "Highlight background face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-modified-active
  '((t (:inherit warning :bold nil)))
  "Modified buffer face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-tab-index-active
  '((t (:inherit zw/modeline-highlight-foreground-active)))
  "Tab index face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-line-column-active
  '((t (:inherit zw/modeline-highlight-foreground-active)))
  "Line-column face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-vc-active
  '((t (:inherit zw/modeline-default-active :bold t)))
  "VC face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-vc-modified-active
  '((t (:inherit zw/modeline-modified-active :bold t)))
  "VC modified face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-encoding-active
  '((t (:inherit zw/modeline-default-active)))
  "Encoding face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-mark-active
  '((t (:inherit zw/modeline-highlight-background-active)))
  "Active mark face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-kmacro-active
  '((t (:inherit zw/modeline-highlight-background-active)))
  "Defining kmacro face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-remote-active
  '((t (:inherit zw/modeline-highlight-background-active)))
  "Remote file face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-env-active
  '((t (:inherit zw/modeline-default-active :bold t)))
  "Environment face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-lsp-active
  '((t (:inherit zw/modeline-default-active :bold t)))
  "LSP mode face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-major-mode-active
  '((t (:inherit zw/modeline-highlight-foreground-active :bold t)))
  "Major mode face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-process-active
  '((t (:inherit zw/modeline-highlight-foreground-active :bold t)))
  "Process face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-input-method-active
  '((t (:inherit zw/modeline-default-active :bold t)))
  "Input method face for active modeline"
  :group 'zw/modeline-active)

(defun zw/modeline-set-face (active-face inactive-face)
  (if (zw/modeline-selected-window-active-p)
      active-face
    inactive-face))

;; keep track of selected window
(defvar zw/modeline--selected-window nil)
(defun zw/modeline--update-selected-window ()
  "Update selected window (before mode-line is active)"
  (setq zw/modeline--selected-window (selected-window)))

;; This hooks is necessary to register selected window because when
;;  a modeline is evaluated, the corresponding window is always selected.
(add-hook 'post-command-hook #'zw/modeline--update-selected-window)

(defun zw/modeline-selected-window-active-p ()
  (let* ((window (get-buffer-window (current-buffer))))
    (eq window zw/modeline--selected-window)))

;; modeline segments
(defvar zw/modeline-separator
  (propertize " " 'face 'zw/modeline-default-active))

(defun zw/modeline-tab-index ()
  (when (length> (tab-bar-tabs) 1)
    (concat
     "<"
     ;; current tab index
     (propertize
      (number-to-string (+ (tab-bar--current-tab-index) 1))
      'face (zw/modeline-set-face 'zw/modeline-tab-index-active 'zw/modeline-default-inactive)
      'help-echo (concat "Current Tab: "
                         (number-to-string (+ 1 (tab-bar--current-tab-index)))))
     ">"
     zw/modeline-separator)))

(defun zw/modeline-buffer-name (zw/modeline-buffer-name-max zw/modeline-buffer-name-ellipse)
  (let* ((file-name (buffer-name))
         (file-name-abbrev (if (length< file-name zw/modeline-buffer-name-max)
                               file-name
                             (truncate-string-to-width
                              file-name zw/modeline-buffer-name-max nil nil
                              zw/modeline-buffer-name-ellipse))))
    (concat
     (propertize file-name-abbrev
                 'face (if (and (buffer-file-name) (buffer-modified-p))
                           (zw/modeline-set-face 'zw/modeline-modified-active 'zw/modeline-default-inactive)
                         (zw/modeline-set-face 'zw/modeline-default-active 'zw/modeline-default-inactive))
                 'help-echo (concat "File: " (buffer-file-name) ", Encoding:" (zw/modeline-encoding)))
     zw/modeline-separator)))

(defun zw/modeline-text-scale ()
  (when (and (boundp 'text-scale-mode-amount)
             (/= text-scale-mode-amount 0))
    (concat
     (propertize
      (format
       (if (> text-scale-mode-amount 0)
           "%+d"
         "%-d")
       text-scale-mode-amount)
      'face (zw/modeline-set-face 'zw/modeline-default-active 'zw/modeline-default-inactive))
     zw/modeline-separator)))

(defun zw/modeline-count-region ()
  (when (region-active-p)
    (let ((num-words (number-to-string (count-words-region (region-beginning) (region-end)))))
      (concat
       (propertize
        (concat num-words "W")
        'face (zw/modeline-set-face 'zw/modeline-line-column-active 'zw/modeline-default-inactive)
        'help-echo (concat "Word counts: " num-words))
       zw/modeline-separator))))

(defun zw/modeline-line-column ()
  (pcase major-mode
    ((pred (lambda (mode) (member mode '(dired-mode
                                         org-agenda-mode
                                         image-mode
                                         eaf-mode
                                         vterm-mode
                                         exwm-mode))))
     "")
    ('pdf-view-mode
     (propertize (concat
                  (number-to-string
                   (pdf-view-current-page))
                  "/"
                  (or
                   (ignore-errors
                     (number-to-string
                      (pdf-cache-number-of-pages)))
                   "???")
                  " ")
                 'face (zw/modeline-set-face 'zw/modeline-line-column-active
                                             'zw/modeline-default-inactive)))
    (_
     (concat
      (propertize "%l:%c %p "
                  'face (zw/modeline-set-face 'zw/modeline-line-column-active 'zw/modeline-default-inactive))
      (zw/modeline-count-region)))))

(defun zw/modeline-encoding ()
  (let* ((sys (coding-system-plist buffer-file-coding-system))
         (cat (plist-get sys :category))
         (sym (if (memq cat
                        '(coding-category-undecided coding-category-utf-8))
                  'utf-8
                (plist-get sys :name)))
         (encoding (upcase (symbol-name sym))))
    (when (not (string= encoding "NO-CONVERSION"))
      (concat
       (propertize
        encoding
        'face (zw/modeline-set-face 'zw/modeline-encoding-active 'zw/modeline-default-inactive))
       zw/modeline-separator))))

(defun zw/modeline-mark-active ()
  (when mark-active
    (concat
     (propertize " Mark "
                 'face (zw/modeline-set-face 'zw/modeline-mark-active 'zw/modeline-default-inactive))
     zw/modeline-separator)))

(defun zw/modeline-kmacro-recording ()
  "Display current Emacs kmacro being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (concat
     (propertize " kmacro "
                 'face (zw/modeline-set-face 'zw/modeline-kmacro-active 'zw/modeline-default-inactive))
     zw/modeline-separator)))

(defun zw/modeline-remote ()
  (when (file-remote-p default-directory)
    (concat
     (propertize (concat " Remote: " (file-remote-p default-directory 'host) " ")
                 'face (zw/modeline-set-face 'zw/modeline-remote-active 'zw/modeline-default-inactive))
     zw/modeline-separator)))

(defun zw/modeline-env ()
  (when (and (featurep 'conda) conda-env-current-name)
    (concat
     (propertize (upcase conda-env-current-name)
                 'face (zw/modeline-set-face 'zw/modeline-env-active
                                             'zw/modeline-default-inactive))
     zw/modeline-separator)))

(defun zw/modeline-vc ()
  (when vc-mode
    (let* ((backend (vc-backend buffer-file-name))
           (vc-info (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
      (concat
       (propertize (concat vc-info)
                   'face (if (vc-up-to-date-p (buffer-file-name (current-buffer)))
                             (zw/modeline-set-face 'zw/modeline-vc-active
                                                   'zw/modeline-default-inactive)
                           (zw/modeline-set-face 'zw/modeline-vc-modified-active
                                                 'zw/modeline-default-inactive)))
       zw/modeline-separator))))

(defun zw/modeline-lsp-bridge ()
  (when (and (featurep 'lsp-bridge) lsp-bridge-mode)
    (when lsp-bridge-server
      (concat
       (propertize "BRIDGE"
                   'help-echo (format "lsp-bridge:%s" lsp-bridge-server-port)
                   'face (zw/modeline-set-face 'zw/modeline-lsp-active 'zw/modeline-default-inactive))
       zw/modeline-separator))))

(defun zw/modeline-lsp ()
  (when (and (featurep 'lsp-mode) lsp-mode)
    (let ((workspaces (lsp-workspaces)))
      (concat
       (propertize "LSP"
                   'face (zw/modeline-set-face 'zw/modeline-lsp-active 'zw/modeline-default-inactive)
                   'help-echo
                   (if workspaces
                       (concat "LSP Connected "
                               (string-join
                                (mapcar (lambda (w)
                                          (format "[%s]\n" (lsp--workspace-print w)))
                                        workspaces)))))
       zw/modeline-separator))))

(defun zw/modeline-eglot ()
  (when (and (featurep 'eglot) (eglot-managed-p))
    (let ((server (eglot-current-server)))
      (concat
       (propertize "EGLOT"
                   'face (zw/modeline-set-face 'zw/modeline-lsp-active 'zw/modeline-default-inactive)
                   'help-echo
                   (if server
                       (concat "EGLOT Connected "
                               (format "[%s/%s]"
                                       (eglot--major-modes server)
                                       (eglot--project-nickname server)))))
       zw/modeline-separator))))

(defun zw/modeline-major-mode ()
  (concat (propertize (format-mode-line mode-name)
                      'face (zw/modeline-set-face 'zw/modeline-major-mode-active 'zw/modeline-default-inactive))
          zw/modeline-separator))

(defun zw/modeline-process ()
  (let ((process (string-trim (format-mode-line mode-line-process))))
    (when (not (length= process 0))
      (concat (propertize process
                          'face (zw/modeline-set-face 'zw/modeline-process-active 'zw/modeline-default-inactive))
              zw/modeline-separator))))

(defun zw/modeline-input-method ()
  (let ((method (string-trim (or current-input-method-title ""))))
    (when (not (length= method 0))
      (concat (propertize method
                          'face (zw/modeline-set-face 'zw/modeline-input-method-active 'zw/modeline-default-inactive))
              zw/modeline-separator))))

(defun zw/modeline-middle-space (rhs)
  (let* ((middle-space (string-pixel-width rhs)))
    (propertize
     " "
     'face (zw/modeline-set-face 'zw/modeline-default-active 'zw/modeline-default-inactive)
     'display `((space :align-to
                       (- (+ right right-fringe right-margin)
                          (,middle-space)))))))

(defun zw/modeline-rhs ()
  (concat
   ;; input method
   (zw/modeline-input-method)
   ;; process
   (zw/modeline-process)
   ;; version control
   (zw/modeline-vc)
   ;; env
   (zw/modeline-env)
   ;; lsp
   (zw/modeline-lsp-bridge)
   (zw/modeline-lsp)
   (zw/modeline-eglot)
   ;; major mode
   (zw/modeline-major-mode)))

;;; main modeline
(setq-default
 mode-line-format
 (list
  "%e"
  " "
  '(:eval (zw/modeline-tab-index))
  ;; the buffer name; the file name as a tool tip
  '(:eval (zw/modeline-buffer-name 30 "..."))
  ;; text scale amount
  '(:eval (zw/modeline-text-scale))
  ;; line and column
  '(:eval (zw/modeline-line-column))
  ;; mark active
  '(:eval (zw/modeline-mark-active))
  ;; record kmacro
  '(:eval (zw/modeline-kmacro-recording))
  ;; is remote file?
  '(:eval (zw/modeline-remote))
  ;; add space between left and right
  '(:eval (zw/modeline-middle-space (zw/modeline-rhs)))
  ;; right hand side of the modeline
  '(:eval (zw/modeline-rhs))
  " "))

;; repl modeline
(dolist (mode '(inferior-ess-mode-hook
                inferior-python-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local mode-line-format
                          (list
                           "%e"
                           " "
                           '(:eval (zw/modeline-tab-index))
                           ;; the buffer name
                           '(:eval (propertize
                                    (zw/modeline-buffer-name 10 "...")
                                    'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                                                'zw/modeline-default-inactive)))
                           ;; text scale amount
                           '(:eval (zw/modeline-text-scale))
                           ;; mark active
                           '(:eval (zw/modeline-mark-active))
                           ;; record kmacro
                           '(:eval (zw/modeline-kmacro-recording))
                           ;; is remote file?
                           '(:eval (zw/modeline-remote))
                           ;; add modeline process
                           '(:eval (zw/modeline-process))
                           ;; env
                           '(:eval (zw/modeline-env))
                           " ")))))

;;; special mode
(add-hook 'ess-mode-hook
          (lambda ()
            (setq-local mode-line-process nil)))
(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (setq-local mode-line-process
                        '(:eval (concat ":run" (nth ess--busy-count ess-busy-strings))))))

(provide 'zw-modeline)