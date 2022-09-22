(defgroup zw-modeline nil
  "zw-modeline"
  :group 'convenience)

(defgroup zw-modeline-active nil
  "zw-modeline-active"
  :group 'zw-modeline)

(defgroup zw-modeline-inactive nil
  "zw-modeline-inactive"
  :group 'zw-modeline)

(defface zw-modeline-default-active
  `((t (:foreground ,(face-foreground 'default))))
  "Default face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-default-inactive
  `((t (:foreground ,(face-foreground 'font-lock-comment-face))))
  "Default face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-file-directory-active
  '((t (:inherit font-lock-keyword-face :bold t)))
  "File directory face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-file-directory-inactive
  '((t (:inherit zw-modeline-default-inactive :bold t)))
  "File directory face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-tab-index-active
  '((t (:inherit font-lock-keyword-face)))
  "Tab index face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-tab-index-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "Tab index face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-read-only-active
  '((t (:foreground "#E94560")))
  "Read only buffer face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-read-only-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "Read only buffer face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-read-write-active
  '((t (:foreground "#76BA99")))
  "Read write buffer face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-read-write-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "Read write buffer face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-modified-active
  '((t (:foreground "#F7A76C")))
  "Modified buffer face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-modified-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "Modified buffer face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-line-column-active
  '((t (:inherit font-lock-constant-face)))
  "Line-column face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-line-column-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "Line-column face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-vc-modified-active
  '((t (:inherit zw-modeline-modified-active)))
  "Line-column face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-vc-modified-inactive
  '((t (:inherit zw-modeline-modified-inactive)))
  "Line-column face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-encoding-active
  '((t (:inherit font-lock-constant-face)))
  "Encoding face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-encoding-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "Encoding face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-remote-active
  '((t (:inherit highlight)))
  "Remote file face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-remote-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "Remote file face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-lsp-active
  '((t (:inherit success)))
  "LSP mode face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-lsp-inactive
  '((t (:inherit zw-modeline-default-inactive)))
  "LSP mode face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-major-mode-active
  '((t (:inherit font-lock-keyword-face :bold t)))
  "Major mode face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-major-mode-inactive
  '((t (:inherit zw-modeline-default-inactive :bold t)))
  "Major mode face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-process-active
  '((t (:inherit font-lock-function-name-face :bold t)))
  "Process face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-process-inactive
  '((t (:inherit zw-modeline-process-active)))
  "Process face for inactive modeline"
  :group 'zw-modeline-inactive)

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
(defun zw/modeline-tab-index ()
  (concat
   "<"
   ;; current tab index
   (propertize
    (number-to-string (+ (tab-bar--current-tab-index) 1))
    'face (zw/modeline-set-face 'zw-modeline-tab-index-active 'zw-modeline-tab-index-inactive)
    'help-echo (concat "Current Tab: "
                       (number-to-string (tab-bar--current-tab-index))))
   ">"))

(defvar zw-modeline-buffer-name-max 30
  "Maximum length of buffer name")
(defvar zw-modeline-buffer-name-ellipse "..."
  "Ellipse for long buffer name")
(defun zw/modeline-buffer-name ()
  (let* ((file-name (buffer-name))
         (file-name-abbrev (if (length< file-name zw-modeline-buffer-name-max)
                               file-name
                             (truncate-string-to-width
                              file-name zw-modeline-buffer-name-max nil nil
                              zw-modeline-buffer-name-ellipse))))
    (propertize file-name-abbrev
                'face (zw/modeline-set-face 'zw-modeline-default-active 'zw-modeline-default-inactive)
                'help-echo (buffer-file-name))))

(defun zw/modeline-buffer-status ()
  "modeline is read-only or modified"
  (if buffer-read-only
      (propertize "RO"
                  'face (zw/modeline-set-face 'zw-modeline-read-only-active 'zw-modeline-read-only-inactive)
                  'help-echo "Buffer is read-only")
    (if (buffer-modified-p)
        (propertize "**"
                    'face (zw/modeline-set-face 'zw-modeline-modified-active 'zw-modeline-modified-inactive)
                    'help-echo "Buffer has been modified")
      (propertize "--"
                  'face (zw/modeline-set-face 'zw-modeline-read-write-active 'zw-modeline-read-write-inactive)
                  'help-echo "Buffer is read/write"))))

(defun zw/modeline-line-column ()
  (pcase major-mode
    ((pred (lambda (mode) (member mode '(dired-mode
                                         org-agenda-mode
                                         image-mode))))
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
                   "???"))
                 'face (zw/modeline-set-face 'zw-modeline-line-column-active
                                             'zw-modeline-line-column-inactive)))
    (_
     (list
      (propertize "%l"
                  'face (zw/modeline-set-face 'zw-modeline-line-column-active 'zw-modeline-line-column-inactive))
      ":"
      (propertize "%c"
                  'face (zw/modeline-set-face 'zw-modeline-line-column-active 'zw-modeline-line-column-inactive))
      " "
      (zw/modeline-count-region)))))

(defun zw/modeline-encoding ()
  (let* ((sys (coding-system-plist buffer-file-coding-system))
         (cat (plist-get sys :category))
         (sym (if (memq cat
                        '(coding-category-undecided coding-category-utf-8))
                  'utf-8
                (plist-get sys :name)))
         (encoding (upcase (symbol-name sym))))
    (if (string= encoding "NO-CONVERSION")
        ""
      (concat
       " "
       (propertize
        encoding
        'face (zw/modeline-set-face 'zw-modeline-encoding-active 'zw-modeline-encoding-inactive))))))

(defun zw/modeline-remote ()
  (if (file-remote-p default-directory)
      (propertize (concat " Remote: " (file-remote-p default-directory 'host) " ")
                  'face (zw/modeline-set-face 'zw-modeline-remote-active 'zw-modeline-remote-inactive))))

(defun zw/modeline-conda ()
  (when (and (featurep 'conda) conda-env-current-name)
    (concat (propertize "CONDA:"
                        'face 'zw-modeline-default-inactive)
            conda-env-current-name)))

(defun zw/modeline-vc ()
  (if vc-mode
      (let* ((backend (vc-backend buffer-file-name))
             (vc-info (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
        (concat
         " "
         (propertize "VC:"
                     'face 'zw-modeline-default-inactive)
         (if (vc-up-to-date-p (buffer-file-name (current-buffer)))
             (concat vc-info "")
           (propertize (concat vc-info "*")
                       'face (zw/modeline-set-face 'zw-modeline-vc-modified-active 'zw-modeline-vc-modified-inactive)))))))

(defun zw/modeline-lsp-bridge ()
  (if (and (featurep 'lsp-bridge) lsp-bridge-mode)
      (when lsp-bridge-server
        (propertize "BRIDGE "
                    'help-echo (format "lsp-bridge:%s" lsp-bridge-server-port)
                    'face (zw/modeline-set-face 'zw-modeline-lsp-active 'zw-modeline-lsp-inactive)))
    ""))

(defun zw/modeline-lsp ()
  (if (and (featurep 'lsp-mode) lsp-mode)
      (let ((workspaces (lsp-workspaces)))
        (concat
         (propertize "LSP"
                     'face (zw/modeline-set-face 'zw-modeline-lsp-active 'zw-modeline-lsp-inactive)
                     'help-echo
                     (if workspaces
                         (concat "LSP Connected "
                                 (string-join
                                  (mapcar (lambda (w)
                                            (format "[%s]\n" (lsp--workspace-print w)))
                                          workspaces)))))
         " "))))

(defun zw/modeline-eglot ()
  (if (and (featurep 'eglot) (eglot-managed-p))
      (let ((server (eglot-current-server)))
        (concat
         (propertize "EGLOT"
                     'face (zw/modeline-set-face 'zw-modeline-lsp-active 'zw-modeline-lsp-inactive)
                     'help-echo
                     (if server
                         (concat "EGLOT Connected "
                                 (format "[%s/%s]"
                                         (eglot--major-mode server)
                                         (eglot--project-nickname server)))))
         " "))))

(defun zw/modeline-major-mode ()
  (propertize (format-mode-line mode-name)
              'face (zw/modeline-set-face 'zw-modeline-major-mode-active 'zw-modeline-major-mode-inactive)))

(defun zw/modeline-count-region ()
  (if (region-active-p)
      (let ((num-words (number-to-string (count-words-region (region-beginning) (region-end)))))
        (propertize
         (concat
          num-words
          "W ")
         'face (zw/modeline-set-face 'zw-modeline-line-column-active 'zw-modeline-line-column-inactive)
         'help-echo (concat "Word counts: " num-words)))
    ""))

(defun zw/modeline-rhs ()
  (concat
   ;; conda env
   (zw/modeline-conda)
   ;; version control
   (zw/modeline-vc)
   ;; encoding
   (zw/modeline-encoding)
   ;; major mode
   " ["
   (zw/modeline-lsp-bridge)
   (zw/modeline-lsp)
   (zw/modeline-eglot)
   (zw/modeline-major-mode)
   "]"))

(defun zw/modeline-middle-space ()
  (propertize
   " " 'display
   `((space :align-to
            (- (+ right right-fringe right-margin)
               ,(+ 2 (apply '+ (list (length (zw/modeline-rhs))))))))))

(defun zw/modeline-set-face (active-face inactive-face)
  (if (zw/modeline-selected-window-active-p)
      active-face
    inactive-face))

(defun zw/modeline-propertize-process-info (process)
  (propertize
   process
   'face (zw/modeline-set-face 'zw-modeline-process-active 'zw-modeline-process-inactive)
   'help-echo (concat (buffer-name) " is running...")))

;;; set modeline
(setq-default
 mode-line-format
 (list
  "%e"
  " "
  '(:eval (zw/modeline-tab-index))
  " "
  ;; is this buffer read-only or modified since the last save?
  '(:eval (zw/modeline-buffer-status))
  " "
  ;; the buffer name; the file name as a tool tip
  '(:eval (zw/modeline-buffer-name))
  " "
  ;; line and column
  '(:eval (zw/modeline-line-column))
  " "
  ;; is remote file?
  '(:eval (zw/modeline-remote))

  ;; add modeline process
  '(:eval mode-line-process)

  ;; add space between left and right
  '(:eval (zw/modeline-middle-space))

  ;; right hand side of the modeline
  " "
  '(:eval (zw/modeline-rhs))
  " "))

;;; misc config
;; repl
(dolist (mode '(inferior-ess-mode-hook
                inferior-python-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local mode-line-format
                          (list
                           "%e"
                           " "
                           '(:eval (zw/modeline-tab-index))
                           " "
                           '(:eval (zw/modeline-buffer-name))
                           " "
                           ;; is remote file?
                           '(:eval (zw/modeline-remote))
                           ;; add modeline process
                           '(:eval mode-line-process)
                           ;; conda env
                           '(:eval (zw/modeline-conda)))))))

;; ess-r
(add-hook 'inferior-ess-mode-hook
          (lambda ()
            (setq-local mode-line-process
                        '(:eval (zw/modeline-propertize-process-info
                                 (nth ess--busy-count ess-busy-strings))))))

(provide 'zw-modeline)
