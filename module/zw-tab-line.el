;; -*- lexical-binding: t -*-

;; * Group
;;;; group hash table
(defvar zw/tab-line-group--hash-table (make-hash-table))

(defun zw/tab-line-group-add-buffer (buffer)
  (when (and (buffer-live-p buffer)
             (zw/tab-line-buffer-group-visible))
    (let* ((group (zw/tab-line-buffer-group buffer))
           (group-buffers (gethash group zw/tab-line-group--hash-table)))
      (add-to-list 'group-buffers buffer 'append)
      (puthash group
               ;; clear dead buffers
               (cl-remove-if-not 'buffer-live-p group-buffers)
               zw/tab-line-group--hash-table))))

(defun zw/tab-line-group-add-current-buffer ()
  (zw/tab-line-group-add-buffer (current-buffer)))

(defun zw/tab-line-group-remove-buffer (buffer)
  (let* ((group (zw/tab-line-buffer-group buffer))
         (group-buffers (gethash group zw/tab-line-group--hash-table)))
    (puthash group
             (cl-remove buffer group-buffers)
             zw/tab-line-group--hash-table)))

(add-hook 'buffer-list-update-hook 'zw/tab-line-group-add-current-buffer)

(with-eval-after-load "polymode"
  (defun zw/tab-line-remove-polymode-inner (base-buffer)
    (cl-map 'list
            'zw/tab-line-group-remove-buffer
            (zw/indirect-buffers base-buffer)))
  (defun zw/tab-line-group-add-buffer-after (after-buffer new-buffer)
    (let* ((group (zw/tab-line-buffer-group new-buffer))
           (group-buffers (gethash group zw/tab-line-group--hash-table)))
      (puthash group (zw/insert-after group-buffers after-buffer new-buffer)
               zw/tab-line-group--hash-table)))
  (add-hook 'polymode-after-switch-buffer-hook
            (lambda (old-buffer new-buffer)
              (zw/tab-line-group-remove-buffer new-buffer)
              (zw/tab-line-group-add-buffer-after old-buffer new-buffer)
              (zw/tab-line-group-remove-buffer old-buffer)))
  (add-hook 'polymode-init-inner-hook
            (lambda ()
              (zw/tab-line-remove-polymode-inner (buffer-base-buffer)))))

;;;; group buffers
(defun zw/tab-line-buffer-group (buffer)
  (with-current-buffer buffer
    (cond (buffer-file-name
           "File")
          ((memq major-mode '(helpful-mode
                              help-mode
                              ess-r-help-mode))
           "Doc")
          ((memq major-mode '(inferior-ess-r-mode
                              inferior-python-mode))
           "REPL")
          (t nil))))

(defun zw/tab-line-buffer-group-visible ()
  (zw/tab-line-buffer-group (current-buffer)))

(defun zw/tab-line-buffer-group-buffers ()
  (let* ((group (zw/tab-line-buffer-group (current-buffer))))
    ;; clear dead buffers and invisible buffers
    (cl-remove-if (lambda (buffer)
                    (or (not (buffer-live-p buffer))
                        (with-current-buffer buffer
                          (string-equal " " (substring (buffer-name) 0 1)))))
                  (gethash group zw/tab-line-group--hash-table))))

;; * Appearence
;; ** face
(defun zw/tab-line-init-appearence ()
  (set-face-attribute 'tab-line-tab-current nil
                      :overline (face-background 'highlight))
  (dolist (face '(tab-line
                  tab-line-tab
                  tab-line-tab-current
		  tab-line-tab-inactive))
    (set-face-attribute face nil
                        :family (face-attribute 'default :font)
                        :height (face-attribute 'tab-bar :height))))

;; fix issue when switching theme
(advice-add 'consult-theme :after (lambda (arg)
                                    (zw/tab-line-init-appearence)
                                    (tab-line-format)))
;; ** tab name
(defun zw/tab-line-tab-name (buffer &optional _buffers)
  (format " %s " (buffer-name buffer)))

(defun zw/tab-line-tab-name-format (orig-fun &rest args)
  (let* ((tab-string (apply orig-fun args))
         (buffer-name (string-trim (string-replace tab-line-close-button "" tab-string)))
         (buffer (get-buffer buffer-name))
         (selected-p (eq buffer (window-buffer)))
         (icon (if (buffer-file-name buffer)
                   (nerd-icons-icon-for-file (buffer-file-name buffer))
                 (with-current-buffer buffer
                   (nerd-icons-icon-for-mode major-mode))))
         (icon-face-raw (get-text-property 0 'face icon))
         (icon-face (if selected-p
                        (if (mode-line-window-selected-p)
                            (list :inherit icon-face-raw
                                  :height (face-attribute 'tab-line-tab-current :height)
                                  :background (face-background 'tab-line-tab-current)
                                  :overline (face-attribute 'tab-line-tab-current :overline))
                          (list :inherit icon-face-raw
                                :height (face-attribute 'tab-line-tab :height)
                                :background (face-background 'tab-line-tab)
                                :overline (face-attribute 'tab-line-tab :overline)))
                      'tab-line-tab-inactive))
         (space-face (if selected-p
                         (if (mode-line-window-selected-p)
                             'tab-line-tab-current
                           'tab-line-tab)
                       'tab-line-tab-inactive))
         (space (propertize " " 'face space-face
                            'keymap tab-line-tab-map
                            'mouse-face 'tab-line-highlight)))
    (concat space
            (propertize icon 'face icon-face
                        'keymap tab-line-tab-map
                        'mouse-face 'tab-line-highlight)
            tab-string
            space)))

(advice-add 'tab-line-tab-name-format-default :around
            'zw/tab-line-tab-name-format)

;; ** bar
(defun zw/tab-line-bar ()
  (let ((color (face-background 'tab-line))
        (width 1)
        (height (floor (* (string-pixel-width " ")
                          2.5))))
    (zw/modeline--begin color width height)))

;; ** debug
(defun zw/tab-line-debug-keymap (function)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'tab-line 'mouse-1)
                (lambda (&optional event)
                  (interactive "e")
                  (with-current-buffer (window-buffer (posn-window (cadr event)))
                    (call-interactively function))))
    map))

(defun zw/tab-line-debug-start ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_alt") " ")
              'face 'success
              'mouse-face 'highlight
              'keymap (zw/tab-line-debug-keymap 'zw/dape)))

(defun zw/tab-line-debug-next ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_line_by_line") " ")
              'face 'warning
              'mouse-face 'highlight
              'keymap (zw/tab-line-debug-keymap 'dape-next)))

(defun zw/tab-line-debug-continue ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_continue_small") " ")
              'face 'warning
              'mouse-face 'highlight
              'keymap (zw/tab-line-debug-keymap 'dape-continue)))

(defun zw/tab-line-debug-quit ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_disconnect") " ")
              'face 'error
              'mouse-face 'highlight
              'keymap (zw/tab-line-debug-keymap 'dape-quit)))

(defun zw/tab-line-debug-rerun ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_rerun") " ")
              'face 'error
              'mouse-face 'highlight
              'keymap (zw/tab-line-debug-keymap 'dape-restart)))

(defun zw/tab-line-debug-rhs ()
  (when (derived-mode-p 'prog-mode)
    (concat (zw/tab-line-debug-start)
            (zw/tab-line-bar)
            (zw/tab-line-debug-next)
            (zw/tab-line-bar)
            (zw/tab-line-debug-continue)
            (zw/tab-line-bar)
            (zw/tab-line-debug-quit)
            (zw/tab-line-bar)
            (zw/tab-line-debug-rerun))))

;; ** template
(defun zw/tab-line-format-template (orig-fun &rest args)
  (let ((strings (apply orig-fun args)))
    (append (list (zw/tab-line-bar))
            strings
            (list (zw/modeline-middle-space (zw/tab-line-debug-rhs)))
            (list (zw/tab-line-debug-rhs)))))

(advice-add 'tab-line-format-template :around
            'zw/tab-line-format-template)

;; * keymap
;; ** select tab
(defun zw/tab-line-select (index)
  (interactive)
  (let* ((visible-tabs (funcall tab-line-tabs-function))
         (n-visible-tabs (length visible-tabs))
         (selected-buffer (nth (- index 1) visible-tabs)))
    (unless (eq (current-buffer) selected-buffer)
      (if (> index n-visible-tabs)
          (message "Tab %s does not exist" index)
        (let* ((selected-window (selected-window))
               (window-dedicated-p (window-dedicated-p selected-window)))
          (switch-to-buffer selected-buffer)
          (set-window-dedicated-p selected-window window-dedicated-p))))))

(dolist (key-func (mapcar (lambda (i)
                            `(,(kbd (format "s-%d" i)) .
                              (lambda ()
                                (interactive)
                                (zw/tab-line-select ,i))))
                          (number-sequence 0 9)))
  (define-key global-map (car key-func) (cdr key-func)))

;; * Config
(with-eval-after-load "tab-line"
  (setq tab-line-tab-name-function #'zw/tab-line-tab-name
        tab-line-tabs-function #'zw/tab-line-buffer-group-buffers
        tab-line-new-button-show nil
        tab-line-close-button-show t
        tab-line-close-button (propertize "×" 'keymap tab-line-tab-close-map
                                          'face 'shadow
                                          'mouse-face 'tab-line-close-highlight
                                          'help-echo "Click to close tab")
        tab-line-close-tab-function #'kill-buffer
        tab-line-separator ""
        x-underline-at-descent-line t))
(add-hook 'tab-line-mode-hook 'zw/tab-line-init-appearence)

;; * enable
(add-hook 'after-init-hook 'global-tab-line-mode)
(defun zw/tab-line-hide ()
  (when (and (featurep 'tab-line)
	     tab-line-mode
             (not (zw/tab-line-buffer-group-visible)))
    (tab-line-mode -1)))
(add-hook 'window-configuration-change-hook 'zw/tab-line-hide)
(add-hook 'after-change-major-mode-hook 'zw/tab-line-hide)
(add-hook 'buffer-list-update-hook 'zw/tab-line-hide)

;; * Provide
(provide 'zw-tab-line)
