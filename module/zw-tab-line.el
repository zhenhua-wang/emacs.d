;; -*- lexical-binding: t -*-

;; * Group
(defcustom zw/tab-line-buffer-group-alist
  '(((memq major-mode '(helpful-mode
                        help-mode
                        ess-r-help-mode))
     . Help)
    ((memq major-mode '(inferior-ess-r-mode
                        inferior-python-mode))
     . REPL)
    (buffer-file-name
     . File))
  "Alist of tab line buffer groups.  (predicate . group).")

(defun zw/tab-line-buffer-group (buffer)
  "Get `group' for buffer."
  (with-current-buffer buffer
    (cdr (cl-find-if
          (lambda (pred-group) (eval (car pred-group)))
          zw/tab-line-buffer-group-alist))))

;; group hash table
(defvar zw/tab-line-group--hash-table (make-hash-table))

(defun zw/tab-line-get-group-buffers (group)
  "Get `group-buffers' for `group' excluding dead buffers."
  (cl-remove-if-not 'buffer-live-p
                    (gethash group zw/tab-line-group--hash-table)))

(defvar zw/tab-line-group--after-index nil)
(defun zw/tab-line-group-save-after-index ()
  ;; store after-index for 'find-alternate-file
  (when (backtrace-frame 0 'find-alternate-file)
    (when-let* ((buffer (current-buffer))
                (group (zw/tab-line-buffer-group buffer))
                (group-buffers (zw/tab-line-get-group-buffers group))
                (current-index (cl-position buffer group-buffers)))
      (setq zw/tab-line-group--after-index (- current-index 1)))))
(add-hook 'kill-buffer-hook 'zw/tab-line-group-save-after-index)

(defun zw/tab-line-group-add-buffer-after (after-buffer new-buffer)
  (when (and (buffer-live-p new-buffer)
             (not (minibufferp))
             (zw/tab-line-buffer-group-visible-p)
             (not (zw/hidden-buffer-p new-buffer)))
    (let* ((group (zw/tab-line-buffer-group new-buffer))
           (group-buffers (zw/tab-line-get-group-buffers group))
           (after-index (or zw/tab-line-group--after-index
                            (cl-position after-buffer group-buffers))))
      (when (not (memq new-buffer group-buffers))
        (setq group-buffers
              (if after-index
                  (zw/insert-at-index group-buffers new-buffer (+ after-index 1))
                (append group-buffers (list new-buffer)))
              ;; reset
              zw/tab-line-group--after-index nil)
        (puthash group group-buffers
                 zw/tab-line-group--hash-table)))))

(defun zw/tab-line-group-add-current-buffer ()
  (let ((buffer (current-buffer)))
    (zw/tab-line-group-add-buffer-after
     (other-buffer buffer t) buffer)))
(add-hook 'buffer-list-update-hook 'zw/tab-line-group-add-current-buffer)
(add-hook 'after-change-major-mode-hook 'zw/tab-line-group-add-current-buffer)

(defun zw/tab-line-group-remove-buffer (buffer)
  (let* ((group (zw/tab-line-buffer-group buffer))
         (group-buffers (zw/tab-line-get-group-buffers group)))
    (puthash group (delq buffer group-buffers)
             zw/tab-line-group--hash-table)))

(defun zw/tab-line-buffer-group-visible-p ()
  (zw/tab-line-buffer-group (current-buffer)))

(defun zw/tab-line-tabs-function ()
  (let* ((group (zw/tab-line-buffer-group (current-buffer))))
    (zw/tab-line-get-group-buffers group)))

;; ** polymode
(with-eval-after-load "polymode"
  (add-hook 'polymode-after-switch-buffer-hook
            (lambda (old-buffer new-buffer)
              (zw/tab-line-group-remove-buffer new-buffer)
              (zw/tab-line-group-add-buffer-after old-buffer new-buffer)
              (zw/tab-line-group-remove-buffer old-buffer)))
  (add-hook 'polymode-init-inner-hook
            (lambda ()
              (cl-map 'list
                      'zw/tab-line-group-remove-buffer
                      (zw/indirect-buffers (buffer-base-buffer))))))

;; * Appearence
;; ** face
(defun zw/tab-line-set-face ()
  (set-face-attribute 'tab-line-tab-current nil
                      :overline (face-background 'highlight nil t))
  (dolist (face '(tab-line
                  tab-line-tab
                  tab-line-tab-current
		  tab-line-tab-inactive))
    (set-face-attribute face nil
                        :family (face-attribute 'default :font)
                        :height (face-attribute 'tab-bar :height))))

(add-hook 'tab-line-mode-hook 'zw/tab-line-set-face)

;; ** tab name
(defun zw/tab-line-tab-name (buffer &optional _buffers)
  (format " %s " (truncate-string-to-width (buffer-name buffer) 20 nil nil "…")))

(defun zw/tab-line-tab-icon (buffer)
  (with-current-buffer buffer
    (if buffer-file-name
        (nerd-icons-icon-for-file buffer-file-name)
      (nerd-icons-icon-for-mode major-mode))))

(defun zw/tab-line-tab-name-format (tab tabs)
  (let* ((buffer-p (bufferp tab))
         (tab-string (funcall tab-line-tab-name-function tab tabs))
         (buffer-name (string-trim (string-replace tab-line-close-button "" tab-string)))
         (buffer (get-buffer tab))
         (selected-p (eq buffer (window-buffer)))
         (window-selected-p (mode-line-window-selected-p))
         (text-face (if selected-p
                        (if window-selected-p
                            'tab-line-tab-current
                          'tab-line-tab)
                      'tab-line-tab-inactive))
         (icon (zw/tab-line-tab-icon buffer))
         (icon-face-raw (get-text-property 0 'face icon))
         (icon-face (if selected-p
                        (if window-selected-p
                            (list :inherit icon-face-raw
                                  :box (face-attribute 'tab-line-tab-current :box nil t)
                                  :height (face-attribute 'tab-line-tab-current :height)
                                  :background (face-background 'tab-line-tab-current nil t)
                                  :overline (face-attribute 'tab-line-tab-current :overline))
                          (list :inherit icon-face-raw
                                :box (face-attribute 'tab-line-tab :box nil t)
                                :height (face-attribute 'tab-line-tab :height)
                                :background (face-background 'tab-line-tab nil t)
                                :overline (face-attribute 'tab-line-tab :overline)))
                      'tab-line-tab-inactive))
         (space (propertize " " 'face text-face
                            'keymap tab-line-tab-map
                            'mouse-face 'tab-line-highlight)))
    (concat space
            (propertize icon 'face icon-face
                        'keymap tab-line-tab-map
                        'mouse-face 'tab-line-highlight)
            (propertize tab-string 'face text-face
                        'keymap tab-line-tab-map
                        'tab tab
                        'mouse-face 'tab-line-highlight)
            space)))

;; ** bar
(defun zw/tab-line-bar ()
  (if (display-graphic-p)
      (let ((color (face-background 'tab-line nil 'default))
            (width 1)
            (height (floor (* (string-pixel-width " ")
                              2.5))))
        (zw/modeline--bar color width height))
    ""))

;; ** debug
(defcustom zw/tab-line-show-debug t
  "Enbale debug on tab-line."
  :type 'boolean)

(defun zw/tab-line-debug-keymap (left-click-func &optional right-click-func)
  (let ((map (make-sparse-keymap)))
    (define-key map (vector 'tab-line 'mouse-1)
                (lambda (&optional event)
                  (interactive "e")
                  (with-current-buffer (window-buffer (posn-window (cadr event)))
                    (call-interactively left-click-func))))
    (when right-click-func
      (define-key map (vector 'tab-line 'down-mouse-3)
                  (lambda (&optional event)
                    (interactive "e")
                    (with-current-buffer (window-buffer (posn-window (cadr event)))
                      (call-interactively right-click-func)))))
    map))

(defun zw/tab-line-debug-start ()
  (propertize (concat " " (nerd-icons-codicon "nf-cod-debug_alt") " ")
              'face 'success
              'mouse-face 'highlight
              'keymap (zw/tab-line-debug-keymap 'zw/dape 'zw/dape-in-path-menu)))

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
  (when (and (display-graphic-p)
             (derived-mode-p 'prog-mode)
             zw/tab-line-show-debug)
    (concat (zw/tab-line-debug-start)
            (zw/tab-line-bar)
            (zw/tab-line-debug-next)
            (zw/tab-line-bar)
            (zw/tab-line-debug-continue)
            (zw/tab-line-bar)
            (zw/tab-line-debug-quit)
            (zw/tab-line-bar)
            (zw/tab-line-debug-rerun)
            "   ")))

;; ** template
(defun zw/tab-line-middle-space (rhs)
  (let ((middle-space (progn
                        (add-face-text-property 0 (length rhs) 'tab-line t rhs)
                        (string-pixel-width rhs))))
    (propertize
     " "
     'display
     `((space :align-to (- (+ right right-fringe right-margin) (,middle-space)))))))

(defun zw/tab-line-format-template (orig-fun &rest args)
  (let ((strings (apply orig-fun args)))
    (append (list (zw/tab-line-bar))
            strings
            (list (zw/tab-line-middle-space (zw/tab-line-debug-rhs)))
            (list (zw/tab-line-debug-rhs)))))

(advice-add 'tab-line-format-template :around
            'zw/tab-line-format-template)

;; * Config
(with-eval-after-load "tab-line"
  (setq tab-line-tab-name-function #'zw/tab-line-tab-name
        tab-line-tab-name-format-function 'zw/tab-line-tab-name-format
        tab-line-tabs-function #'zw/tab-line-tabs-function
        tab-line-new-button-show nil
        tab-line-close-button-show t
        tab-line-close-button (propertize "" 'keymap tab-line-tab-close-map
                                          'help-echo "Click to close tab")
        tab-line-left-button (propertize "" 'keymap tab-line-left-map
                                         'mouse-face 'highlight)
        tab-line-right-button (propertize "" 'keymap tab-line-right-map
                                          'mouse-face 'highlight)
        tab-line-close-tab-function #'kill-buffer
        tab-line-separator ""
        x-underline-at-descent-line t))

;; * Enable
(defun zw/tab-line-show ()
  (when (zw/tab-line-buffer-group-visible-p)
    (tab-line-mode 1)))
(defun zw/tab-line-init ()
  (require 'tab-line)
  (add-hook 'buffer-list-update-hook 'zw/tab-line-show)
  (add-hook 'after-change-major-mode-hook 'zw/tab-line-show)
  (add-hook 'after-revert-hook 'zw/tab-line-show)
  ;; fix issue when switching theme
  (advice-add 'zw/theme-load-ui :after (lambda ()
                                         (zw/tab-line-set-face)
                                         (tab-line-format)
                                         (tab-line-force-update t))))
(add-hook 'after-init-hook 'zw/tab-line-init)

;; * Drag move
(defun zw/tab-line-mouse-move-tab (event)
  "Move a tab to a different position on the tab line.
This command should be bound to a drag event.  It moves the tab
at the mouse-down event to the position at mouse-up event."
  (interactive "e")
  (let* ((posnp1 (tab-line-event-start event))
         (posnp2 (event-end event))
         (string1 (car (posn-string posnp1)))
         (string2 (car (posn-string posnp2)))
         (buffer1 (when string1 (tab-line--get-tab-property 'tab string1)))
         (buffer2 (when string2 (tab-line--get-tab-property 'tab string2)))
         (window1 (posn-window posnp1))
         (window2 (posn-window posnp2))
         (group (zw/tab-line-buffer-group buffer1))
         (buffers (zw/tab-line-get-group-buffers group))
         (pos1 (when buffer1 (seq-position buffers buffer1)))
         (pos2 (when buffer2 (seq-position buffers buffer2))))
    (when (and (eq window1 window2) buffer1 pos2)
      (setq buffers (delq buffer1 buffers))
      (cl-pushnew buffer1 (nthcdr pos2 buffers))
      (puthash group buffers zw/tab-line-group--hash-table)
      (set-window-parameter window1 'tab-line-cache nil)
      (with-selected-window window1 (force-mode-line-update))
      (message "move %s p:%s to %s p:%s" string1 (+ pos1 1) string2 (+ pos2 1)))))

(with-eval-after-load "tab-line"
  (keymap-set tab-line-tab-map "<tab-line> <drag-mouse-1>" #'zw/tab-line-mouse-move-tab))

;; * Keymap
;; kill buffer and switch
(defcustom zw/tab-line-kill-buffer-and-switch-type 'next
  "Determines which buffer to switch to after killing the current one.

- If set to `next`, switch to the next buffer in the tab line.
- If set to `previous`, switch to the previous buffer."
  :type '(choice (const :tag "Switch to next buffer" next)
                 (const :tag "Switch to previous buffer" previous)))

(defvar zw/tab-line-before-kill-buffer-hook nil)
(defun zw/tab-line-kill-buffer-and-switch ()
  (interactive)
  (run-hooks 'zw/tab-line-before-kill-buffer-hook)
  (let ((buffer-switch))
    (when-let* ((buffer (current-buffer))
                (group (zw/tab-line-buffer-group buffer))
                (group-buffers (zw/tab-line-get-group-buffers group))
                (max-index (- (length group-buffers) 1))
                (pos (cl-position buffer group-buffers))
                (pos-previous (- pos 1))
                (pos-next (+ pos 1))
                (buffer-pos (cond
                             ((eq zw/tab-line-kill-buffer-and-switch-type 'previous)
                              (if (< pos-previous 0) 1 pos-previous))
                             ((eq zw/tab-line-kill-buffer-and-switch-type 'next)
                              (if (> pos-next max-index) pos-previous pos-next)))))
      (setq buffer-switch (nth buffer-pos group-buffers)))
    (call-interactively 'kill-current-buffer)
    (when (and buffer-switch (buffer-live-p buffer-switch))
      (switch-to-buffer buffer-switch))))

;; select tab
(defun zw/tab-line-select (index)
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
                          (number-sequence 1 8)))
  (define-key global-map (car key-func) (cdr key-func)))

(bind-keys :map global-map
           ("s-q" . zw/tab-line-kill-buffer-and-switch)
           ("s-{" . tab-line-switch-to-prev-tab)
           ("s-}" . tab-line-switch-to-next-tab)
           ("s-9" . (lambda ()
                      (interactive)
                      (zw/tab-line-select
                       (length (funcall tab-line-tabs-function))))))

;; * Provide
(provide 'zw-tab-line)
