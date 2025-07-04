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
  `((t (:foreground ,(face-foreground 'shadow))))
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
  '((t (:inherit zw/modeline-default-active)))
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

(defface zw/modeline-vc-untracked-active
  '((t (:inherit vc-removed-state :bold t)))
  "VC untracked face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-encoding-active
  '((t (:inherit zw/modeline-default-active)))
  "Encoding face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-mark-count
  '((t (:inherit zw/modeline-highlight-background-active)))
  "Active mark face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-kmacro-active
  '((t (:inherit zw/modeline-highlight-background-active)))
  "Defining kmacro face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-local-active
  '((t (:inherit zw/modeline-highlight-background-active)))
  "Local file face for active modeline"
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
  '((t (:inherit zw/modeline-default-active)))
  "Environment face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-lsp-active
  '((t (:inherit zw/modeline-highlight-foreground-active :bold t)))
  "LSP mode face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-major-mode-active
  '((t (:inherit zw/modeline-highlight-foreground-active :bold t)))
  "Major mode face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-process-active
  '((t (:inherit zw/modeline-default-active :bold t)))
  "Process face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-input-method-active
  '((t (:inherit zw/modeline-default-active :bold t)))
  "Input method face for active modeline"
  :group 'zw/modeline-active)

(defface zw/modeline-separator-active `((t :background ,(face-background 'mode-line)))
  "Modeline separator active face."
  :group 'zw/modeline)

(defun zw/modeline-set-face (active-face inactive-face)
  (if (mode-line-window-selected-p)
      active-face
    inactive-face))

;; * Module
;; ** bar
(defun zw/modeline--bar (color width height)
  (if (and (display-graphic-p)
           (image-type-available-p 'pbm))
      (propertize
       " " 'display
       (ignore-errors
         (create-image
          (concat (format "P1\n%i %i\n" width height)
                  (make-string (* width height) ?1)
                  "\n")
          'pbm t :scale 1 :foreground color :ascent 'center)))
    (propertize " " 'face `(:background ,color))))

(defun zw/modeline-bar ()
  (if (display-graphic-p)
      (let ((color (if (mode-line-window-selected-p)
                       (face-background 'zw/modeline-separator-active nil 'default)
                     (face-background 'mode-line-inactive nil 'default)))
            (width 1)
            (height (floor (* (string-pixel-width " ")
                              2.7))))
        (zw/modeline--bar color width height))
    " "))

;; ** seperator
(defvar zw/modeline-separator
  (propertize " " 'face 'zw/modeline-default-active))

(defun zw/modeline-separator-thin ()
  (if (display-graphic-p)
      (let ((color (if (mode-line-window-selected-p)
                       (face-background 'zw/modeline-separator-active nil 'default)
                     (face-background 'mode-line-inactive nil 'default)))
            (width (floor (/ (string-pixel-width " ") 4)))
            (height (string-pixel-width " ")))
        (zw/modeline--bar color width height))
    zw/modeline-separator))

;; ** remote
(defcustom zw/modeline-remote-show-local t
  "Whether show local indicator on mode line."
  :type 'boolean)

(defun zw/modeline-remote ()
  (let ((icon (nerd-icons-codicon
               "nf-cod-remote"
               :height 1
               :v-adjust 0.05)))
    (concat
     (if (file-remote-p default-directory)
         (propertize (concat " " icon " " (file-remote-p default-directory 'host) " ")
                     'face (zw/modeline-set-face 'zw/modeline-remote-active 'zw/modeline-remote-inactive))
       (when zw/modeline-remote-show-local
         (propertize (concat " " icon " ")
                     'face (zw/modeline-set-face 'zw/modeline-local-active 'zw/modeline-remote-inactive))))
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
(defun zw/modeline-buffer-name (&optional fullname-p truncate-p)
  (let* ((max-length (floor (* 0.3 (window-width))))
         (ellipse "...")
         (file-name (buffer-file-name))
         (buffer-name (if (and fullname-p file-name) (abbreviate-file-name file-name) (buffer-name)))
         (buffer-name-abbrev (if (and truncate-p (length> buffer-name max-length))
                                 (truncate-string-to-width buffer-name max-length nil nil ellipse)
                               buffer-name)))
    (concat
     " "
     (propertize buffer-name-abbrev
                 'face (if (and (buffer-file-name) (buffer-modified-p))
                           (zw/modeline-set-face 'zw/modeline-modified-active 'zw/modeline-default-inactive)
                         (zw/modeline-set-face 'zw/modeline-buffer-name-active 'zw/modeline-default-inactive))
                 'help-echo (concat "Encoding:" (zw/modeline-encoding) ", File: " buffer-file-truename))
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

;; ** Read only
(defun zw/modeline-read-only ()
  (when buffer-read-only
    (concat (propertize (nerd-icons-mdicon
                         "nf-md-lock"
                         :height 1
                         :v-adjust 0.05)
                        'face (zw/modeline-set-face 'warning 'zw/modeline-default-inactive))
            zw/modeline-separator)))

;; ** line column
(defvar zw/modeline-line-column-show-p nil)

(defun zw/modeline-line-column-toggle ()
  "Toggle line column on mode line"
  (interactive)
  (setq zw/modeline-line-column-show-p
        (if zw/modeline-line-column-show-p nil t)))

(defun zw/modeline-line-column ()
  (cond
   ((member major-mode '(org-agenda-mode
                         image-mode
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
   (t (if zw/modeline-line-column-show-p
          (propertize "%l:%c %p "
                      'face (zw/modeline-set-face 'zw/modeline-line-column-active
                                                  'zw/modeline-default-inactive))
        ""))))

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

;; ** mark count
(defun zw/modeline-mark-count ()
  (when (and mark-active (mode-line-window-selected-p))
    (let* ((num-words (count-words-region (region-beginning) (region-end)))
           (plural (if (> num-words 1) "s" ""))
           (text (if (> (length (buffer-substring (region-beginning) (region-end))) 0)
                     (format " %i word%s" num-words plural) (if (display-graphic-p) "" " "))))
      (concat
       (propertize (format " %s%s "
                           (nerd-icons-mdicon "nf-md-sticker_text"
                                              :v-adjust 0.05)
                           text)
                   'face (zw/modeline-set-face 'zw/modeline-mark-count 'zw/modeline-default-inactive))
       zw/modeline-separator))))

;; ** kmacro recording
(defun zw/modeline-kmacro-recording ()
  "Display current Emacs kmacro being recorded."
  (when (or defining-kbd-macro executing-kbd-macro)
    (concat
     (propertize (format " %s kmacro "
                         (nerd-icons-mdicon "nf-md-record_circle"
                                            :v-adjust 0.05))
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
  (let ((current-project (project-current)))
    (when (and buffer-file-name current-project)
      (let* ((backend (vc-backend buffer-file-name))
             (state (vc-state buffer-file-name backend))
             (branch (when vc-mode (substring-no-properties vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
             (icon-face (cond
                         ((eq state 'up-to-date)
                          `(,(nerd-icons-devicon "nf-dev-git_branch") . zw/modeline-vc-active))
                         ((not branch)
                          `(,(nerd-icons-octicon "nf-oct-git_pull_request_closed") . zw/modeline-vc-untracked-active))
                         (t
                          `(,(nerd-icons-devicon "nf-dev-git_compare") . zw/modeline-vc-modified-active)))))
        (concat
         (propertize (concat (car icon-face)
                             (zw/modeline-separator-thin)
                             (project-name (project-current))
                             "/"
                             (or branch "Untracked"))
                     'face (zw/modeline-set-face (cdr icon-face)
                                                 'zw/modeline-default-inactive)
                     'help-echo (format "VC root: %s" (project-root current-project)))
         zw/modeline-separator)))))

;; ** LSP
(defun zw/modeline-lsp ()
  (when (and (featurep 'lsp-mode) lsp-mode)
    (let ((workspaces (lsp-workspaces)))
      (concat
       (propertize (nerd-icons-codicon "nf-cod-rocket" :height 1 :v-adjust 0.05)
                   'face (zw/modeline-set-face 'zw/modeline-lsp-active 'zw/modeline-default-inactive)
                   'help-echo
                   (if workspaces
                       (concat "LSP Connected "
                               (string-join
                                (mapcar (lambda (w)
                                          (format "[%s]" (lsp--workspace-print w)))
                                        workspaces)))))
       (when (not (display-graphic-p)) " ")
       zw/modeline-separator))))

(defun zw/modeline-eglot ()
  (when (and (featurep 'eglot) (eglot-managed-p))
    (let ((server (eglot-current-server)))
      (concat
       (propertize (nerd-icons-codicon "nf-cod-rocket" :height 1 :v-adjust 0.05)
                   'face (zw/modeline-set-face 'zw/modeline-lsp-active 'zw/modeline-default-inactive)
                   'help-echo
                   (if server
                       (concat "EGLOT Connected "
                               (format "[%s/%s]"
                                       (eglot--major-modes server)
                                       (eglot--project-nickname server)))))
       (when (not (display-graphic-p)) " ")
       zw/modeline-separator))))

;; ** major mode
(defun zw/modeline-major-mode ()
  (concat (propertize (format-mode-line mode-name)
                      'face (zw/modeline-set-face 'zw/modeline-major-mode-active 'zw/modeline-default-inactive))
          zw/modeline-separator))

;; ** process
(defvar zw/modeline--process nil)

(defun zw/modeline-process ()
  (let ((process (string-trim (format-mode-line zw/modeline--process))))
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
  (when (and (featurep 'flymake) flymake-mode)
    (concat
     (let* ((errors (format-mode-line (flymake--mode-line-counter :error)))
            (warnings (format-mode-line (flymake--mode-line-counter :warning)))
            (num-errors (string-to-number errors))
            (num-warnings (string-to-number warnings))
            (keymap (let ((map (make-sparse-keymap)))
                      (define-key map (vector 'mode-line 'mouse-1) 'flymake-start)
                      map)))
       (if (and (= num-errors 0) (= num-warnings 0))
           (concat (propertize (nerd-icons-octicon
                                "nf-oct-check"
                                :height 1
                                :v-adjust 0.05)
                               'face (zw/modeline-set-face 'success 'zw/modeline-default-inactive)
                               'mouse-face 'highlight
                               'keymap keymap)
                   (if (display-graphic-p) "" " "))
         (concat (propertize (nerd-icons-faicon
                              "nf-fa-times_circle"
                              :height 1
                              :v-adjust 0.05)
                             'face (zw/modeline-set-face 'error 'zw/modeline-default-inactive)
                             'mouse-face 'highlight
                             'keymap keymap)
                 (zw/modeline-separator-thin)
                 (propertize (string-trim errors) 'face (zw/modeline-set-face 'zw/modeline-default-active
                                                                              'zw/modeline-default-inactive))
                 (zw/modeline-separator-thin)
                 (propertize (nerd-icons-faicon
                              "nf-fa-warning"
                              :height 1
                              :v-adjust 0.05)
                             'face (zw/modeline-set-face 'warning 'zw/modeline-default-inactive)
                             'mouse-face 'highlight
                             'keymap keymap)
                 (zw/modeline-separator-thin)
                 (propertize (string-trim warnings) 'face (zw/modeline-set-face 'zw/modeline-default-active
                                                                                'zw/modeline-default-inactive)))))
     zw/modeline-separator)))

;; ** middle space
(defun zw/modeline-middle-space (rhs)
  (let* ((middle-space (progn
                         (add-face-text-property 0 (length rhs) 'mode-line t rhs)
                         (string-pixel-width rhs)))
         (vertical-border-p (not (or (display-graphic-p)
                                     (window-at-side-p (selected-window) 'right))))
         (vertical-border-width (if vertical-border-p 1 0)))
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin)
                           (,vertical-border-width)
                           (,middle-space)))))))

;; ** modeline right hand side
(defun zw/modeline-rhs ()
  (concat
   (zw/modeline-input-method)
   (zw/modeline-process)
   (zw/modeline-vc)
   (zw/modeline-major-mode)))

;; * Config
;; ** main
(defun zw/modeline-init ()
  (setq-default
   mode-line-format
   (list
    "%e"
    '(:eval (zw/modeline-bar))
    ;; left
    '(:eval (zw/modeline-remote))
    '(:eval (zw/modeline-buffer-name nil :truncate))
    (if (display-graphic-p) "" zw/modeline-separator)
    '(:eval (zw/modeline-text-scale))
    '(:eval (zw/modeline-read-only))
    '(:eval (zw/modeline-line-column))
    '(:eval (zw/modeline-lsp))
    '(:eval (zw/modeline-eglot))
    '(:eval (zw/modeline-flymake))
    '(:eval (zw/modeline-mark-count))
    '(:eval (zw/modeline-kmacro-recording))
    ;; right
    '(:eval (zw/modeline-middle-space (zw/modeline-rhs)))
    '(:eval (zw/modeline-rhs)))))
(add-hook 'after-init-hook 'zw/modeline-init)

;; ** repl
(dolist (mode '(inferior-ess-mode-hook
                inferior-python-mode-hook))
  (add-hook mode
            (lambda ()
              (setq-local mode-line-format
                          (list
                           "%e"
                           '(:eval (zw/modeline-bar))
                           '(:eval (zw/modeline-remote))
                           '(:eval (propertize
                                    (zw/modeline-buffer-name)
                                    'face (zw/modeline-set-face 'zw/modeline-major-mode-active
                                                                'zw/modeline-default-inactive)))
                           '(:eval (zw/modeline-text-scale))
                           '(:eval (zw/modeline-mark-count))
                           '(:eval (zw/modeline-kmacro-recording))
                           '(:eval (zw/modeline-process)))))))

;; ** ring bell
(defvar zw/modeline--ring-bell-timer nil)

(defun zw/modeline-ring-bell ()
  (let* ((buf (current-buffer))
         (bell-color (face-foreground 'error)))
    ;; clear timer
    (when zw/modeline--ring-bell-timer
      (cancel-timer zw/modeline--ring-bell-timer)
      (setq zw/modeline--ring-bell-timer nil))
    ;; ring bell
    (face-remap-add-relative 'mode-line :background bell-color)
    (set-face-background 'zw/modeline-separator-active bell-color)
    (force-mode-line-update)
    ;; set timer
    (setq zw/modeline--ring-bell-timer
          (run-with-timer 0.15 nil
                          (lambda ()
                            (with-current-buffer buf
                              (setq face-remapping-alist
                                    (cl-remove-if (lambda (cookie)
                                                    (eq (car cookie) 'mode-line))
                                                  face-remapping-alist))
                              (set-face-background 'zw/modeline-separator-active (face-background 'mode-line))
                              (force-mode-line-update)))))))

(setq ring-bell-function 'zw/modeline-ring-bell
      visible-bell t)

;; * Provide
(provide 'zw-mode-line)
