(defgroup nano-modeline nil
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
  '((t (:inherit font-lock-comment-face)))
  "Default face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-tab-index-active
  '((t (:inherit font-lock-keyword-face)))
  "Tab index face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-tab-index-inactive
  '((t (:inherit font-lock-comment-face)))
  "Tab index face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-read-only-active
  '((t (:foreground "#E94560")))
  "Read only buffer face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-read-only-inactive
  '((t (:inherit font-lock-comment-face)))
  "Read only buffer face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-read-write-active
  '((t (:foreground "#76BA99")))
  "Read write buffer face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-read-write-inactive
  '((t (:inherit font-lock-comment-face)))
  "Read write buffer face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-modified-active
  '((t (:foreground "#F7A76C")))
  "Modified buffer face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-modified-inactive
  '((t (:inherit font-lock-comment-face)))
  "Modified buffer face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-line-column-active
  '((t (:inherit font-lock-type-face)))
  "Line-column face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-line-column-inactive
  '((t (:inherit font-lock-comment-face)))
  "Line-column face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-encoding-active
  '((t (:inherit font-lock-constant-face)))
  "Encoding face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-encoding-inactive
  '((t (:inherit font-lock-comment-face)))
  "Encoding face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-remote-active
  '((t (:inherit highlight)))
  "Remote file face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-remote-inactive
  '((t (:inherit font-lock-comment-face)))
  "Remote file face for inactive modeline"
  :group 'zw-modeline-inactive)

(defface zw-modeline-major-mode-active
  '((t (:inherit font-lock-keyword-face)))
  "Major mode face for active modeline"
  :group 'zw-modeline-active)

(defface zw-modeline-major-mode-inactive
  '((t (:inherit font-lock-comment-face)))
  "Major mode face for inactive modeline"
  :group 'zw-modeline-inactive)

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
  '(:eval
    (concat
     " <"
     ;; current tab index
     (propertize
      (number-to-string (+ (tab-bar--current-tab-index) 1))
      'face (zw/modeline-set-face 'zw-modeline-tab-index-active 'zw-modeline-tab-index-inactive)
      'help-echo (concat "Current Tab: "
                         (number-to-string (tab-bar--current-tab-index))))
     ">")))

(defun zw/modeline-encoding ()
  (let* ((sys (coding-system-plist buffer-file-coding-system))
         (cat (plist-get sys :category))
         (sym (if (memq cat
                        '(coding-category-undecided coding-category-utf-8))
                  'utf-8
                (plist-get sys :name))))
    (concat
     " "
     (propertize
      (upcase (symbol-name sym))
      'face (zw/modeline-set-face 'zw-modeline-encoding-active 'zw-modeline-encoding-inactive)))))

(defun zw/modeline-conda ()
  (when (and (featurep 'conda) conda-env-current-name)
      (concat (propertize "CONDA:"
                          'face 'font-lock-comment-face)
              conda-env-current-name)))

(defun zw/modeline-vc ()
  (if vc-mode
      (let ((backend (vc-backend buffer-file-name)))
        (concat
         " "
         (propertize "VC:"
                     'face 'font-lock-comment-face)
         (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))
         (if (vc-up-to-date-p (buffer-file-name (current-buffer)))
             " "
           "!")))))

(defun zw/modeline-major-mode ()
  (concat
   " ["
   (propertize (format-mode-line mode-name)
               'face (zw/modeline-set-face 'zw-modeline-major-mode-active 'zw-modeline-major-mode-inactive))
   "]"))

(defun zw/modeline-rhs ()
  (concat
   ;; conda env
   (zw/modeline-conda)
   ;; version control
   (zw/modeline-vc)
   ;; encoding
   (zw/modeline-encoding)
   ;; major mode
   (zw/modeline-major-mode)))

(defun zw/string-width (str)
  (if str (string-width str) 0))

(defun zw/modeline-set-face (active-face inactive-face)
  (if (zw/modeline-selected-window-active-p)
      active-face
    inactive-face))

;;; set modeline
(setq-default
 mode-line-format
 (list
  "%e"
  " "
  ;; is this buffer read-only or modified since the last save?
  '(:eval
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
                      'help-echo (buffer-file-name)))))
  " "
  ;; the buffer name; the file name as a tool tip
  '(:eval (propertize "%b"
                      'face (zw/modeline-set-face 'zw-modeline-default-active 'zw-modeline-default-inactive)
                      'help-echo (buffer-file-name)))
  " "
  ;; line and column
  '(:eval
    (list
     (propertize "%l"
                 'face (zw/modeline-set-face 'zw-modeline-line-column-active 'zw-modeline-line-column-inactive))
     ":"
     (propertize "%c"
                 'face (zw/modeline-set-face 'zw-modeline-line-column-active 'zw-modeline-line-column-inactive))))
  " "
  ;; is remote file?
  '(:eval (if (file-remote-p default-directory)
              (propertize "Remote"
                          'face (zw/modeline-set-face 'zw-modeline-remote-active 'zw-modeline-remote-inactive))))

  ;; add space between left and right
  '(:eval
    (propertize
     " " 'display
     `((space :align-to
              (- (+ right right-fringe right-margin)
                 ,(+ 2 (apply '+ (list (zw/string-width (zw/modeline-rhs))))))))))

  ;; right hand side of the modeline
  " "
  '(:eval (zw/modeline-rhs))
  " "))

(provide 'zw-modeline)
