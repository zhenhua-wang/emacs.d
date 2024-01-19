;; -*- lexical-binding: t -*-

;; * Face
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

(defface zw/modeline-highlight-background-inactive
  '((t (:inherit zw/modeline-default-inactive)))
  "Highlight background face for inactive modeline"
  :group 'zw/modeline-inactive)

(defface zw/modeline-buffer-name-active
  '((t (:inherit zw/modeline-default-active :bold t)))
  "buffer name face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-modified-active
  '((t (:inherit warning :bold t)))
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

(defface zw/modeline-remote-inactive
  '((t (:inherit zw/modeline-highlight-background-inactive)))
  "Remote file face for inactive modeline"
  :group 'zw/modeline-inactive)

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

(defun zw/modeline-window-active-p ()
  (let* ((window (get-buffer-window (current-buffer))))
    (eq window zw/active-window)))

(defun zw/modeline-set-face (active-face inactive-face)
  (if (zw/modeline-window-active-p)
      active-face
    inactive-face))

;; * Module
;; ** seperator
(defvar zw/modeline-separator
  (propertize " " 'face 'zw/modeline-default-active))

;; ** begin
(defun zw/modeline-begin ()
  (if (image-type-available-p 'pbm)
      (let ((color (if (zw/modeline-window-active-p)
                       (face-background 'mode-line-highlight)
                     (face-background 'zw/modeline-highlight-background-inactive)))
            (width (string-pixel-width " "))
            (height 50))
        (propertize
         " " 'display
         (ignore-errors
           (create-image
            (concat (format "P1\n%i %i\n" width height)
                    (make-string (* width height) ?1)
                    "\n")
            'pbm t :scale 1 :foreground color :ascent 'center))))
    " "))

;; ** remote
(defun zw/modeline-remote ()
  (if (file-remote-p default-directory)
      (concat
       (propertize (concat " " (file-remote-p default-directory 'host) " ")
                   'face (zw/modeline-set-face 'zw/modeline-remote-active 'zw/modeline-remote-inactive))
       zw/modeline-separator)
    (concat
     (propertize " "
                 'face (zw/modeline-set-face 'zw/modeline-remote-active 'zw/modeline-remote-inactive))
     zw/modeline-separator)))

;; ** tab index
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

;; ** buffer name
(defun zw/modeline-buffer-name (zw/modeline-buffer-name-max zw/modeline-buffer-name-ellipse)
  (let* ((file-name (buffer-name))
         (file-name-abbrev (if (length< file-name zw/modeline-buffer-name-max)
                               file-name
                             (truncate-string-to-width
                              file-name zw/modeline-buffer-name-max nil nil
                              zw/modeline-buffer-name-ellipse))))
    (concat
     " "
     (propertize file-name-abbrev
                 'face (if (and (buffer-file-name) (buffer-modified-p))
                           (zw/modeline-set-face 'zw/modeline-modified-active 'zw/modeline-default-inactive)
                         (zw/modeline-set-face 'zw/modeline-buffer-name-active 'zw/modeline-default-inactive))
                 'help-echo (concat "File: " (buffer-file-name) ", Encoding:" (zw/modeline-encoding)))
     " "
     zw/modeline-separator)))

;; ** text scale
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

;; ** count region
(defun zw/modeline-count-region ()
  (when (region-active-p)
    (let ((num-words (number-to-string (count-words-region (region-beginning) (region-end)))))
      (concat
       (propertize
        (concat num-words "W")
        'face (zw/modeline-set-face 'zw/modeline-line-column-active 'zw/modeline-default-inactive)
        'help-echo (concat "Word counts: " num-words))
       zw/modeline-separator))))

;; ** line column
(defun zw/modeline-line-column ()
  (cond
   ((member major-mode '(org-agenda-mode
                         image-mode
                         vterm-mode
                         exwm-mode))
    "")
   ((eq major-mode 'dired-mode)
    (propertize (let* ((info-line-number (if (eq dired-free-space 'separate) 2 1))
                       (total-line-number (- (save-excursion
                                               (goto-char (point-max)) (backward-char)
                                               (line-number-at-pos))
                                             info-line-number))
                       (current-line-number (- (line-number-at-pos) info-line-number)))
                  (format "%s/%d "
                          (if (and (> current-line-number 0)
                                   (<= current-line-number total-line-number))
                              current-line-number
                            "*")
                          total-line-number))
                'face (zw/modeline-set-face 'zw/modeline-line-column-active
                                            'zw/modeline-default-inactive)))
   ((eq major-mode 'pdf-view-mode)
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
   (t
    (propertize "%l:%c %p "
                'face (zw/modeline-set-face 'zw/modeline-line-column-active
                                            'zw/modeline-default-inactive)))))

;; ** encoding
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

;; ** mark active
(defun zw/modeline-mark-active ()
  (when mark-active
    (concat
     (propertize " Mark "
                 'face (zw/modeline-set-face 'zw/modeline-mark-active 'zw/modeline-default-inactive))
     zw/modeline-separator)))

;; ** kmacro recording
(defun zw/modeline-kmacro-recording ()
  "Display current Emacs kmacro being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (concat
     (propertize " kmacro "
                 'face (zw/modeline-set-face 'zw/modeline-kmacro-active 'zw/modeline-default-inactive))
     zw/modeline-separator)))

;; ** env
(defun zw/modeline-env ()
  (when (and (featurep 'conda) conda-env-current-name)
    (concat
     (propertize (upcase conda-env-current-name)
                 'face (zw/modeline-set-face 'zw/modeline-env-active
                                             'zw/modeline-default-inactive))
     zw/modeline-separator)))

;; ** VC
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

;; ** LSP
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

;; ** major mode
(defun zw/modeline-major-mode ()
  (concat (propertize (format-mode-line mode-name)
                      'face (zw/modeline-set-face 'zw/modeline-major-mode-active 'zw/modeline-default-inactive))
          zw/modeline-separator))

;; ** process
(defun zw/modeline-process ()
  (let ((process (string-trim (format-mode-line mode-line-process))))
    (when (not (length= process 0))
      (concat (propertize process
                          'face (zw/modeline-set-face 'zw/modeline-process-active 'zw/modeline-default-inactive))
              zw/modeline-separator))))

;; ** input method
(defun zw/modeline-input-method ()
  (let ((method (string-trim (or current-input-method-title ""))))
    (when (not (length= method 0))
      (concat (propertize method
                          'face (zw/modeline-set-face 'zw/modeline-input-method-active 'zw/modeline-default-inactive))
              zw/modeline-separator))))

;; ** flymake
(defun zw/modeline-flymake ()
  (when flymake-mode
    (concat
     (propertize (format-mode-line flymake-mode-line-title)
                 'face (zw/modeline-set-face 'zw/modeline-default-active 'zw/modeline-default-inactive))
     " "
     (let* ((errors (format-mode-line (flymake--mode-line-counter :error)))
            (warnings (format-mode-line (flymake--mode-line-counter :warning)))
            (num-errors (string-to-number errors))
            (num-warnings (string-to-number warnings)))
       (if (and (= num-errors 0) (= num-warnings 0))
           (propertize ""
                       'face (zw/modeline-set-face 'success 'zw/modeline-default-inactive))
         (concat (propertize errors
                             'face (zw/modeline-set-face 'error 'zw/modeline-default-inactive))
                 (propertize warnings
                             'face (zw/modeline-set-face 'warning 'zw/modeline-default-inactive)))))
     zw/modeline-separator)))

;; ** keycast
(defun zw/modeline-keycast ()
  (when (and zw/modeline-keycast-mode
             (zw/modeline-window-active-p))
    (concat (propertize
             (concat " "(key-description keycast--this-command-keys) " ")
             'face 'keycast-key)
            " "
            (propertize
             (symbol-name keycast--this-command-desc)
             'face 'keycast-command)
            zw/modeline-separator)))

(define-minor-mode zw/modeline-keycast-mode
  "zw/modeline-keycast-mode."
  :global t
  (if zw/modeline-keycast-mode
      (progn (use-package keycast)
             (add-hook 'post-command-hook #'keycast--update)
             (add-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit))
    (progn (remove-hook 'post-command-hook #'keycast--update)
           (remove-hook 'minibuffer-exit-hook #'keycast--minibuffer-exit))))

;; ** middle space
(defun zw/modeline-middle-space (rhs)
  (let ((middle-space (progn
                        (add-face-text-property 0 (length rhs) 'mode-line t rhs)
                        (string-pixel-width rhs))))
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin) (,middle-space)))))))

;; ** modeline right hand side
(defun zw/modeline-rhs ()
  (concat
   (zw/modeline-keycast)
   (zw/modeline-input-method)
   (zw/modeline-process)
   (zw/modeline-vc)
   (zw/modeline-env)
   (zw/modeline-lsp-bridge)
   (zw/modeline-lsp)
   (zw/modeline-eglot)
   (zw/modeline-major-mode)))

;; * Config
;; ** main
(setq-default
 mode-line-format
 (list
  "%e"
  '(:eval (zw/modeline-begin))
  ;; left
  '(:eval (zw/modeline-remote))
  '(:eval (zw/modeline-buffer-name 30 "..."))
  '(:eval (zw/modeline-flymake))
  '(:eval (zw/modeline-text-scale))
  '(:eval (zw/modeline-count-region))
  '(:eval (zw/modeline-mark-active))
  '(:eval (zw/modeline-kmacro-recording))
  ;; right
  '(:eval (zw/modeline-middle-space (zw/modeline-rhs)))
  '(:eval (zw/modeline-rhs))))

;; ** repl
(dolist (mode '(inferior-ess-mode-hook
                inferior-python-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local mode-line-format
                          (list
                           "%e"
                           '(:eval (zw/modeline-begin))
                           '(:eval (zw/modeline-remote))
                           '(:eval (propertize
                                    (zw/modeline-buffer-name 30 "...")
                                    'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                                                'zw/modeline-default-inactive)))
                           '(:eval (zw/modeline-text-scale))
                           '(:eval (zw/modeline-count-region))
                           '(:eval (zw/modeline-mark-active))
                           '(:eval (zw/modeline-kmacro-recording))
                           '(:eval (zw/modeline-process))
                           '(:eval (zw/modeline-env)))))))

;; * Provide
(provide 'zw-modeline)
