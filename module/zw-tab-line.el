;; -*- lexical-binding: t -*-

;; * Group
;;;; group hash table
(defvar zw/tab-line-group--hash-table (make-hash-table))

(defun zw/tab-line-group-add-buffer (buffer)
  (when (and (buffer-live-p buffer)
             (not (minibufferp))
             (zw/tab-line-buffer-group-visible))
    (let* ((group (zw/tab-line-buffer-group buffer))
           (group-buffers (gethash group zw/tab-line-group--hash-table))
           (other-buffer (other-buffer buffer t)))
      (when (not (memq buffer group-buffers))
        (setq group-buffers
              (if (memq other-buffer group-buffers)
                  (zw/insert-after group-buffers other-buffer buffer)
                (append group-buffers (list buffer))))
        (puthash group
                 ;; clear dead buffers
                 (cl-remove-if-not 'buffer-live-p group-buffers)
                 zw/tab-line-group--hash-table)))))

(defun zw/tab-line-group-add-current-buffer ()
  (zw/tab-line-group-add-buffer (current-buffer)))

(defun zw/tab-line-group-remove-buffer (buffer)
  (let* ((group (zw/tab-line-buffer-group buffer))
         (group-buffers (gethash group zw/tab-line-group--hash-table)))
    (puthash group
             (cl-remove buffer group-buffers)
             zw/tab-line-group--hash-table)))

(defun zw/tab-line-switch-to-previous-buffer (buffer group-buffers)
  (when-let* ((max-index (- (length group-buffers) 1))
              (pos (cl-position buffer group-buffers))
              (pos-previous (- pos 1))
              (pos-next (+ pos 1))
              (buffer-pos (if (> pos-next max-index) pos-previous pos-next)))
    (switch-to-buffer (nth buffer-pos group-buffers))))

(defcustom zw/tab-line-kill-buffer-switch-to-previous t
  "Switch to previous buffer on tab-line after kill buffer"
  :type 'boolean)

(defun zw/tab-line-kill-buffer-switch-to-previous ()
  (when zw/tab-line-kill-buffer-switch-to-previous
    (when-let* ((buffer (current-buffer))
                (group (zw/tab-line-buffer-group buffer))
                (group-buffers (gethash group zw/tab-line-group--hash-table)))
      (zw/tab-line-switch-to-previous-buffer
       buffer (cl-remove-if-not 'buffer-live-p group-buffers)))))

(add-hook 'buffer-list-update-hook 'zw/tab-line-group-add-current-buffer)
(add-hook 'kill-buffer-hook 'zw/tab-line-kill-buffer-switch-to-previous)

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
    (cond ((memq major-mode '(helpful-mode
                              help-mode
                              ess-r-help-mode))
           "Help")
          ((memq major-mode '(inferior-ess-r-mode
                              inferior-python-mode))
           "REPL")
          (buffer-file-name
           "File")
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

;; fix issue when switching theme
(advice-add 'consult-theme :after (lambda (arg)
                                    (zw/tab-line-set-face)
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
  (if (display-graphic-p)
      (let ((color (face-background 'tab-line nil 'default))
            (width 1)
            (height (floor (* (string-pixel-width " ")
                              2.5))))
        (zw/modeline--bar color width height))
    ""))

;; ** debug
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
  (when (derived-mode-p 'prog-mode)
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
(add-hook 'tab-line-mode-hook 'zw/tab-line-set-face)

;; * Enable
(global-tab-line-mode 1)
(defun zw/tab-line-hide ()
  (when (and (featurep 'tab-line)
             tab-line-mode
             (not (zw/tab-line-buffer-group-visible)))
    (tab-line-mode -1)))
(add-hook 'buffer-list-update-hook 'zw/tab-line-hide)

(defun zw/tab-line-window-hide (window)
  (with-selected-window window
    (zw/tab-line-hide)))
(defun zw/tab-line-init ()
  (add-hook 'window-buffer-change-functions 'zw/tab-line-window-hide nil t))
(add-hook 'tab-line-mode-hook 'zw/tab-line-init)

;; * Drag move
(defun tab-line-mouse-move-tab (event)
  "Move a tab to a different position on the tab line.
This command should be bound to a drag event.  It moves the tab
at the mouse-down event to the position at mouse-up event."
  (interactive "e")
  (let* ((from-str (posn-string (event-start event)))
         (to-str (posn-string (event-end event)))
	 (from-rowcol (posn-col-row (event-start event)))
	 (to-rowcol (posn-col-row (event-end event)))
	 (from (tab-line--get-tab-property 'tab (car from-str)))
         (to (tab-line--get-tab-property 'tab (car to-str)))
         (group (zw/tab-line-buffer-group (get-buffer from)))
         (group-buffers (gethash group zw/tab-line-group--hash-table)))
    ;; Only adjust if the two tabs are different
    ;; if going left to right add on the right and vice versa if going right to left
    (ignore-errors
      (unless (or (eq from to) (eq from t) (eq to t))
        (puthash group
	         (reverse (let (value)
		            (dolist (elt group-buffers value)
			      ;; add the element in its new position moving leftwards
			      (if (and (equal elt (get-buffer to)) (> (car from-rowcol) (car to-rowcol)))
			          (setq value (cons (get-buffer from) value)))
			      ;; add all other elements in old position
			      (if (not (equal elt (get-buffer from)))
			          (setq value (cons elt value)))
			      ;; add the element in its new position moving rightwards
			      (if (and (equal elt (get-buffer to)) (>= (car to-rowcol) (car from-rowcol)))
			          (setq value (cons (get-buffer from) value)))
			      )))
                 zw/tab-line-group--hash-table)
        (message "move %s p:%s to %s p:%s" from-str (car from-rowcol) to-str (car to-rowcol))
        (force-mode-line-update)))))

(keymap-set tab-line-tab-map "<tab-line> <drag-mouse-1>" #'tab-line-mouse-move-tab)

;; * Keymap
;; select tab
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

;; * Provide
(provide 'zw-tab-line)
